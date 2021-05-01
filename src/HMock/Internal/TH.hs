{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module HMock.Internal.TH where

import Control.Monad
import Control.Monad.Extra
import Data.Char
import Data.Generics
import Data.Maybe
import HMock.Internal.Core
import HMock.Internal.Predicates
import Language.Haskell.TH hiding (match)

unappliedName :: Type -> Maybe Name
unappliedName (AppT a _) = unappliedName a
unappliedName (ConT a) = Just a
unappliedName _ = Nothing

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
  \(ClassD _ _ binders _ _) -> return (fst $ matchVars t binders)
  where
    matchVars (ConT _) vs = ([], vs)
    matchVars (AppT a b) vs = case matchVars a vs of
      (tbl, v : vs') -> ((toName v, b) : tbl, vs')
      (tbl, []) -> (tbl, [])
    matchVars _ vs = ([], vs)

    toName :: TyVarBndr -> Name
    toName (PlainTV n) = n
    toName (KindedTV n _) = n

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
    go (ForallT vs _ _) bound = ([], map boundVar vs ++ bound)
    go _ bound = ([], bound)

    boundVar (PlainTV v) = v
    boundVar (KindedTV v _) = v

hasNiceFields :: Method -> Q Bool
hasNiceFields method = allM isNiceField (methodArgs method)
  where
    isNiceField :: Type -> Q Bool
    isNiceField ty
      | not (null (freeTypeVars ty)) = return False
      | otherwise = (&&) <$> isInstance ''Eq [ty] <*> isInstance ''Show [ty]

makeMockable :: Q Type -> Q [Dec]
makeMockable qt = (++) <$> deriveMockable qt <*> deriveForMockT qt

deriveMockable :: Q Type -> Q [Dec]
deriveMockable qt = do
  t <- qt
  methods <- getMethods t

  when (null methods) $ do
    fail $
      "Cannot derive Mockable because " ++ pprint t
        ++ " has no members in mtl MonadFoo style."

  mockableDecs <-
    sequenceA
      [ defineActionType t methods,
        defineMatcherType t methods,
        defineShowAction methods,
        defineShowMatcher methods,
        defineMatch methods
      ]
  let mockableInst =
        [InstanceD Nothing [] (AppT (ConT ''Mockable) t) mockableDecs]

  exact <- allM hasNiceFields methods
  exactMockableInst <-
    if exact
      then do
        exactDecs <-
          sequenceA
            [ defineExactly methods
            ]
        return
          [InstanceD Nothing [] (AppT (ConT ''ExactMockable) t) exactDecs]
      else return []

  return (mockableInst ++ exactMockableInst)

defineActionType :: Type -> [Method] -> DecQ
defineActionType t methods = do
  a <- newName "a"
  conDecs <- traverse (actionConstructor t) methods
  return
    ( DataInstD
        []
        Nothing
        (AppT (AppT (ConT ''Action) t) (VarT a))
        Nothing
        conDecs
        []
    )

actionConstructor :: Type -> Method -> ConQ
actionConstructor t method
  | null (methodTyVars method) && null (methodCxt method) = return body
  | otherwise =
    forallC (methodTyVars method) (return (methodCxt method)) (return body)
  where
    target = AppT (AppT (ConT ''Action) t) (methodResult method)
    s = Bang NoSourceUnpackedness NoSourceStrictness
    body =
      GadtC
        [methodToActionName (methodName method)]
        (map (s,) (methodArgs method))
        target

methodToActionName :: Name -> Name
methodToActionName name = mkName (toUpper c : cs)
  where
    (c : cs) = nameBase name

defineMatcherType :: Type -> [Method] -> Q Dec
defineMatcherType t methods = do
  a <- newName "a"
  conDecs <- traverse (matcherConstructor t) methods
  return
    ( DataInstD
        []
        Nothing
        (AppT (AppT (ConT ''Matcher) t) (VarT a))
        Nothing
        conDecs
        []
    )

matcherConstructor :: Type -> Method -> ConQ
matcherConstructor t method = return body
  where
    body =
      GadtC
        [methodToMatcherName (methodName method)]
        ( (Bang NoSourceUnpackedness NoSourceStrictness,) . mkPredicate
            <$> methodArgs method
        )
        target
    target =
      AppT
        (AppT (ConT ''Matcher) t)
        (methodResult method)
    mkPredicate argTy
      | null (methodTyVars method) && null (methodCxt method) =
        AppT (ConT ''Predicate) argTy
      | otherwise = ForallT tyVars cx (AppT (ConT ''Predicate) argTy)
      where
        (tyVars, cx) =
          relevantContext argTy (methodTyVars method, methodCxt method)

relevantContext :: Type -> ([TyVarBndr], Cxt) -> ([TyVarBndr], Cxt)
relevantContext ty (tvs, cx) =
  (filter (tvHasVar free) tvs, filter (cxtHasVar free) cx)
  where
    free = freeTypeVars ty
    tvHasVar vars (PlainTV v) = v `elem` vars
    tvHasVar vars (KindedTV v _) = v `elem` vars
    cxtHasVar vars t = any (`elem` vars) (freeTypeVars t)

methodToMatcherName :: Name -> Name
methodToMatcherName name = mkName (toUpper c : cs ++ "_")
  where
    (c : cs) = nameBase name

defineShowAction :: [Method] -> Q Dec
defineShowAction methods = do
  clauses <- traverse showActionClause methods
  return (FunD 'showAction clauses)

showActionClause :: Method -> Q Clause
showActionClause method = do
  argVars <- replicateM (length (methodArgs method)) (newName "p")
  printedArgs <- traverse showArg (zip (methodArgs method) argVars)
  let body =
        NormalB
          ( AppE
              (VarE 'unwords)
              ( ListE
                  ( LitE (StringL (nameBase (methodName method))) :
                    printedArgs
                  )
              )
          )
  return
    ( Clause
        [ ConP
            (methodToActionName (methodName method))
            (VarP <$> argVars)
        ]
        body
        []
    )
  where
    showArg (ty, var) = do
      showable <-
        if null (freeTypeVars ty) then isInstance ''Show [ty] else return False
      return $
        if showable then AppE (VarE 'show) (VarE var) else LitE (StringL "_")

defineShowMatcher :: [Method] -> Q Dec
defineShowMatcher methods = do
  clauses <- concatMapM showMatcherClauses methods
  return (FunD 'showMatcher clauses)

showMatcherClauses :: Method -> Q [Clause]
showMatcherClauses method = do
  argVars <- replicateM (length (methodArgs method)) (newName "a")
  argTVars <- replicateM (length (methodArgs method)) (newName "t")
  predVars <- replicateM (length (methodArgs method)) (newName "p")
  printedArgs <- traverse showArg (zip3 predVars argTVars (methodArgs method))
  printedPolyArgs <- traverse showPolyArg (zip predVars (methodArgs method))
  let body args =
        NormalB
          ( AppE
              (VarE 'unwords)
              ( ListE
                  ( LitE (StringL (nameBase (methodName method))) :
                    args
                  )
              )
          )
  return
    [ Clause
        [ ConP 'Just [ConP (methodToActionName (methodName method)) (typedArg <$> zip3 argVars argTVars (methodArgs method))],
          ConP (methodToMatcherName (methodName method)) (VarP <$> predVars)
        ]
        (body printedArgs)
        [],
      Clause
        [ ConP 'Nothing [],
          ConP (methodToMatcherName (methodName method)) (VarP <$> predVars)
        ]
        (body printedPolyArgs)
        []
    ]
  where
    typedArg (a, t, ty)
      | null (freeTypeVars ty) = VarP a
      | otherwise = SigP (VarP a) (VarT t)

    showArg (p, t, ty)
      | null (freeTypeVars ty) = [|"«" ++ showPredicate $(varE p) ++ "»"|]
      | otherwise = [|"«" ++ showPredicate ($(varE p) :: Predicate $(varT t)) ++ "»"|]

    showPolyArg (p, ty)
      | null (freeTypeVars ty) = [|"«" ++ showPredicate $(varE p) ++ "»"|]
      | otherwise = [|"«polymorphic»"|]

defineMatch :: [Method] -> Q Dec
defineMatch methods = do
  clauses <- (++ fallthrough) <$> traverse matchClause methods
  return (FunD 'match clauses)
  where
    fallthrough
      | length methods <= 1 = []
      | otherwise = [Clause [WildP, WildP] (NormalB (ConE 'NoMatch)) []]

matchClause :: Method -> Q Clause
matchClause method = do
  let n = length (methodArgs method)
  argVars <- replicateM n ((,) <$> newName "p" <*> newName "a")
  mismatchVar <- newName "mismatches"
  clause
    [ conP
        (methodToMatcherName (methodName method))
        (varP . fst <$> argVars),
      conP (methodToActionName (methodName method)) (varP . snd <$> argVars)
    ]
    ( guardedB
        [ (,) <$> normalG [|$(varE mismatchVar) == 0|] <*> [|FullMatch Refl|],
          (,) <$> normalG [|otherwise|] <*> [|PartialMatch Refl $(varE mismatchVar)|]
        ]
    )
    [ valD
        (varP mismatchVar)
        (normalB [|length (filter not $(listE (mkAccept <$> argVars)))|])
        []
    ]
  where
    mkAccept (p, a) = [|accept $(return (VarE p)) $(return (VarE a))|]

defineExactly :: [Method] -> Q Dec
defineExactly methods = do
  clauses <- traverse exactlyClause methods
  return (FunD 'exactly clauses)

exactlyClause :: Method -> Q Clause
exactlyClause method = do
  argVars <- replicateM (length (methodArgs method)) (newName "p")
  return
    ( Clause
        [ConP (methodToActionName (methodName method)) (VarP <$> argVars)]
        ( NormalB
            (makeBody (ConE (methodToMatcherName (methodName method))) argVars)
        )
        []
    )
  where
    makeBody e [] = e
    makeBody e (v : vs) = makeBody (AppE e (AppE (VarE 'eq_) (VarE v))) vs

deriveForMockT :: Q Type -> Q [Dec]
deriveForMockT qt = do
  t <- qt
  members <- getMembers t
  methods <- getMethods t
  when (length methods < length members) $
    fail $
      "Cannot derive MockT because " ++ pprint t
        ++ " has members that don't match mtl MonadFoo style."
  m <- newName "m"
  decs <- traverse mockMethodImpl methods
  return
    [ InstanceD
        Nothing
        [AppT (ConT ''Typeable) (VarT m), AppT (ConT ''Monad) (VarT m)]
        (AppT t (AppT (ConT ''MockT) (VarT m)))
        decs
    ]

mockMethodImpl :: Method -> Q Dec
mockMethodImpl method = do
  argVars <- replicateM (length (methodArgs method)) (newName "p")
  return
    ( FunD
        (methodName method)
        [ Clause
            (VarP <$> argVars)
            ( NormalB
                ( AppE
                    (VarE 'mockMethod)
                    ( actionExp
                        (UnboundVarE (methodToActionName (methodName method)))
                        argVars
                    )
                )
            )
            []
        ]
    )
  where
    actionExp e [] = e
    actionExp e (v : vs) = actionExp (AppE e (VarE v)) vs
