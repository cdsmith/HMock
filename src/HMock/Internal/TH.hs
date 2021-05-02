{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module HMock.Internal.TH where

import Control.Monad
import Control.Monad.Extra
import Data.Char
import Data.Default
import Data.Generics
import Data.Maybe
import HMock.Internal.Core
import HMock.Internal.Predicates
import Language.Haskell.TH hiding (match)
import Language.Haskell.TH.Syntax

newtype MockableOptions = MockableOptions
  { mockPrefix :: String
  }

instance Default MockableOptions where
  def = MockableOptions {mockPrefix = ""}

checkExts :: [Extension] -> Q ()
checkExts = mapM_ checkExt
  where
    checkExt e = do
      enabled <- isExtEnabled e
      unless enabled $
        fail $ "Please enable " ++ show e ++ " to generate this mock."

unappliedName :: Type -> Maybe Name
unappliedName (AppT a _) = unappliedName a
unappliedName (ConT a) = Just a
unappliedName _ = Nothing

tvName :: TyVarBndr -> Name
tvName (PlainTV name) = name
tvName (KindedTV name _) = name

withClass :: Type -> (Dec -> Q a) -> Q a
withClass t f = do
  case unappliedName t of
    Just cls -> do
      info <- reify cls
      case info of
        ClassI dec@ClassD {} _ -> f dec
        _ -> fail $ "Expected " ++ show cls ++ " to be a class, but it wasn't."
    _ -> fail "Expected a class, but got something else."

getClassVars :: Type -> Q [(Name, Type)]
getClassVars t = withClass t $
  \(ClassD _ _ binders _ _) -> fst <$> matchVars t binders
  where
    matchVars (ConT _) vs = return ([], vs)
    matchVars (AppT a b) vs = matchVars a vs >>= \case
      (tbl, v : vs') ->
        checkExts [FlexibleInstances] >> return ((tvName v, b) : tbl, vs')
      (tbl, []) -> return (tbl, [])
    matchVars _ vs = return ([], vs)

substTypeVars :: [(Name, Type)] -> Type -> Type
substTypeVars classVars = everywhere (mkT subst)
  where
    subst (VarT x) | Just t <- lookup x classVars = t
    subst t = t

getMembers :: Type -> Q [Dec]
getMembers t = withClass t $ \(ClassD _ _ _ _ members) -> return members

data Method = Method
  { methodName :: Name,
    methodTyVars :: [TyVarBndr],
    methodCxt :: Cxt,
    methodArgs :: [Type],
    methodResult :: Type
  }

getMethods :: Type -> Q [Method]
getMethods t = mapMaybe . parseMethod <$> getClassVars t <*> getMembers t

parseMethod :: [(Name, Type)] -> Dec -> Maybe Method
parseMethod classVars (SigD name ty)
  | (tvs, cx, argsAndReturn) <- splitType (substTypeVars classVars ty),
    AppT (VarT _) result <- last argsAndReturn =
    Just (Method name tvs cx (init argsAndReturn) result)
  where
    splitType :: Type -> ([TyVarBndr], [Pred], [Type])
    splitType (ForallT tv cx b) =
      let (tvs, cxs, parts) = splitType b in (tv ++ tvs, cx ++ cxs, parts)
    splitType (AppT (AppT ArrowT a) b) =
      let (tvs, cx, parts) = splitType b in (tvs, cx, a : parts)
    splitType r = ([], [], [r])
parseMethod _ _ = Nothing

freeTypeVars :: Type -> [Name]
freeTypeVars = everythingWithContext [] (++) (mkQ ([],) go)
  where
    go (VarT v) bound
      | v `elem` bound = ([], bound)
      | otherwise = ([v], bound)
    go (ForallT vs _ _) bound = ([], map tvName vs ++ bound)
    go _ bound = ([], bound)

hasNiceFields :: Method -> Q Bool
hasNiceFields method = allM isNiceField (methodArgs method)
  where
    isNiceField ty
      | not (null (freeTypeVars ty)) = return False
      | otherwise = (&&) <$> isInstance ''Eq [ty] <*> isInstance ''Show [ty]

makeMockable :: Q Type -> Q [Dec]
makeMockable = makeMockableWithOptions def

makeMockableWithOptions :: MockableOptions -> Q Type -> Q [Dec]
makeMockableWithOptions options qt =
  (++) <$> deriveMockableWithOptions options qt
    <*> deriveForMockTWithOptions options qt

deriveMockable :: Q Type -> Q [Dec]
deriveMockable = deriveMockableWithOptions def

deriveMockableWithOptions :: MockableOptions -> Q Type -> Q [Dec]
deriveMockableWithOptions options qt = do
  checkExts [GADTs, TypeFamilies]

  t <- qt
  methods <- getMethods t

  when (null methods) $ do
    fail $
      "Cannot derive Mockable because " ++ pprint t
        ++ " has no members in mtl MonadFoo style."

  let mockableInst =
        instanceD
          (pure [])
          [t|Mockable $(pure t)|]
          [ defineActionType options t methods,
            defineMatcherType options t methods,
            defineShowAction options methods,
            defineShowMatcher options methods,
            defineMatch options methods
          ]

  exact <- allM hasNiceFields methods
  let exactMockableInst
        | exact =
          [ instanceD
              (pure [])
              [t|ExactMockable $(pure t)|]
              [defineExactly options methods]
          ]
        | otherwise = []

  sequenceA (mockableInst : exactMockableInst)

defineActionType :: MockableOptions -> Type -> [Method] -> DecQ
defineActionType options t methods =
  dataInstD
    (pure [])
    ''Action
    [pure t]
    (Just (AppT (AppT ArrowT StarT) StarT))
    (actionConstructor options t <$> methods)
    []

actionConstructor :: MockableOptions -> Type -> Method -> ConQ
actionConstructor options t method
  | null (methodTyVars method) && null (methodCxt method) = body
  | otherwise = forallC (methodTyVars method) (return (methodCxt method)) body
  where
    body =
      gadtC
        [getActionName options method]
        [ return (Bang NoSourceUnpackedness NoSourceStrictness, argTy)
          | argTy <- methodArgs method
        ]
        [t|Action $(pure t) $(pure (methodResult method))|]

getActionName :: MockableOptions -> Method -> Name
getActionName options method = mkName (mockPrefix options ++ toUpper c : cs)
  where
    (c : cs) = nameBase (methodName method)

defineMatcherType :: MockableOptions -> Type -> [Method] -> Q Dec
defineMatcherType options t methods =
  dataInstD
    (pure [])
    ''Matcher
    [pure t]
    (Just (AppT (AppT ArrowT StarT) StarT))
    (matcherConstructor options t <$> methods)
    []

matcherConstructor :: MockableOptions -> Type -> Method -> ConQ
matcherConstructor options t method =
  gadtC
    [getMatcherName options method]
    [ (Bang NoSourceUnpackedness NoSourceStrictness,) <$> mkPredicate argTy
      | argTy <- methodArgs method
    ]
    [t|Matcher $(pure t) $(pure (methodResult method))|]
  where
    mkPredicate argTy
      | null tyVars && null cx = [t|Predicate $(pure argTy)|]
      | otherwise = do
        checkExts [RankNTypes]
        forallT tyVars (pure cx) [t|Predicate $(pure argTy)|]
      where
        (tyVars, cx) =
          relevantContext argTy (methodTyVars method, methodCxt method)

getMatcherName :: MockableOptions -> Method -> Name
getMatcherName options name =
  mkName (mockPrefix options ++ toUpper c : cs ++ "_")
  where
    (c : cs) = nameBase (methodName name)

relevantContext :: Type -> ([TyVarBndr], Cxt) -> ([TyVarBndr], Cxt)
relevantContext ty (tvs, cx) =
  (filter (tvHasVar free) tvs, filter (cxtHasVar free) cx)
  where
    free = freeTypeVars ty
    tvHasVar vars tv = tvName tv `elem` vars
    cxtHasVar vars t = any (`elem` vars) (freeTypeVars t)

defineShowAction :: MockableOptions -> [Method] -> Q Dec
defineShowAction options methods =
  funD 'showAction (showActionClause options <$> methods)

showActionClause :: MockableOptions -> Method -> Q Clause
showActionClause options method = do
  argVars <- replicateM (length (methodArgs method)) (newName "a")
  clause
    [conP (getActionName options method) (varP <$> argVars)]
    ( normalB
        [|
          unwords
            ( $(lift (nameBase (methodName method))) :
              $(listE (zipWith showArg (methodArgs method) argVars))
            )
          |]
    )
    []
  where
    showArg ty var
      | not (null (freeTypeVars ty)) = fallback ty
      | otherwise = do
        showable <- isInstance ''Show [ty]
        if showable then [|showsPrec 11 $(varE var) ""|] else fallback ty
    fallback ty = [|"(_ :: " ++ $(lift (pprint ty)) ++ ")"|]

defineShowMatcher :: MockableOptions -> [Method] -> Q Dec
defineShowMatcher options methods = do
  clauses <- concatMapM (showMatcherClauses options) methods
  funD 'showMatcher clauses

showMatcherClauses :: MockableOptions -> Method -> Q [ClauseQ]
showMatcherClauses options method = do
  argVars <- replicateM (length (methodArgs method)) (newName "a")
  argTVars <- replicateM (length (methodArgs method)) (newName "t")
  predVars <- replicateM (length (methodArgs method)) (newName "p")
  let actionArgs = zipWith3 actionArg argVars argTVars (methodArgs method)
  let matcherArgs = varP <$> predVars
  let printedArgs = zipWith3 printedArg predVars argTVars (methodArgs method)
  let printedPolyArgs = zipWith printedPolyArg predVars (methodArgs method)
  let body name args = normalB [|unwords ($(lift name) : $(listE args))|]
  return
    [ clause
        [ conP 'Just [conP (getActionName options method) actionArgs],
          conP (getMatcherName options method) matcherArgs
        ]
        (body (nameBase (methodName method)) printedArgs)
        [],
      clause
        [ conP 'Nothing [],
          conP (getMatcherName options method) matcherArgs
        ]
        (body (nameBase (methodName method)) printedPolyArgs)
        []
    ]
  where
    actionArg a t ty
      | null (freeTypeVars ty) = varP a
      | otherwise = checkExts [ScopedTypeVariables] >> sigP (varP a) (varT t)

    printedArg p t ty
      | null (freeTypeVars ty) = [|"«" ++ showPredicate $(varE p) ++ "»"|]
      | otherwise =
        [|"«" ++ showPredicate ($(varE p) :: Predicate $(varT t)) ++ "»"|]

    printedPolyArg p ty
      | null (freeTypeVars ty) = [|"«" ++ showPredicate $(varE p) ++ "»"|]
      | otherwise = [|"«polymorphic»"|]

defineMatch :: MockableOptions -> [Method] -> Q Dec
defineMatch options methods = funD 'match clauses
  where
    clauses = (matchClause options <$> methods) ++ fallthrough
    fallthrough
      | length methods <= 1 = []
      | otherwise = [clause [wildP, wildP] (normalB [|NoMatch|]) []]

matchClause :: MockableOptions -> Method -> Q Clause
matchClause options method = do
  argVars <-
    replicateM
      (length (methodArgs method))
      ((,) <$> newName "p" <*> newName "a")
  mmVar <- newName "mismatches"
  clause
    [ conP
        (getMatcherName options method)
        (varP . fst <$> argVars),
      conP (getActionName options method) (varP . snd <$> argVars)
    ]
    ( guardedB
        [ (,) <$> normalG [|$(varE mmVar) == 0|] <*> [|FullMatch Refl|],
          (,) <$> normalG [|otherwise|] <*> [|PartialMatch Refl $(varE mmVar)|]
        ]
    )
    [ valD
        (varP mmVar)
        (normalB [|length (filter not $(listE (mkAccept <$> argVars)))|])
        []
    ]
  where
    mkAccept (p, a) = [|accept $(return (VarE p)) $(return (VarE a))|]

defineExactly :: MockableOptions -> [Method] -> Q Dec
defineExactly options methods =
  funD 'exactly (exactlyClause options <$> methods)

exactlyClause :: MockableOptions -> Method -> Q Clause
exactlyClause options method = do
  argVars <- replicateM (length (methodArgs method)) (newName "a")
  clause
    [conP (getActionName options method) (varP <$> argVars)]
    (normalB (makeBody argVars (conE (getMatcherName options method))))
    []
  where
    makeBody [] e = e
    makeBody (v : vs) e = makeBody vs [|$e (eq_ $(varE v))|]

deriveForMockT :: Q Type -> Q [Dec]
deriveForMockT = deriveForMockTWithOptions def

deriveForMockTWithOptions :: MockableOptions -> Q Type -> Q [Dec]
deriveForMockTWithOptions options qt = do
  t <- qt
  members <- getMembers t
  methods <- getMethods t

  when (length methods < length members) $
    fail $
      "Cannot derive MockT because " ++ pprint t
        ++ " has members that don't match mtl MonadFoo style."

  m <- newName "m"
  let decs = map (mockMethodImpl options) methods
  sequenceA
    [ instanceD
        ( sequenceA
            [ [t|Typeable $(varT m)|],
              [t|Monad $(varT m)|]
            ]
        )
        [t|$qt (MockT $(varT m))|]
        decs
    ]

mockMethodImpl :: MockableOptions -> Method -> Q Dec
mockMethodImpl options method = do
  argVars <- replicateM (length (methodArgs method)) (newName "a")
  funD
    (methodName method)
    [ clause
        (varP <$> argVars)
        ( normalB
            [|
              mockMethod
                $( actionExp
                     argVars
                     (unboundVarE (getActionName options method))
                 )
              |]
        )
        []
    ]
  where
    actionExp [] e = e
    actionExp (v : vs) e = actionExp vs [|$e $(varE v)|]
