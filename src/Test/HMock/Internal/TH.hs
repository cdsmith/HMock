{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Test.HMock.Internal.TH where

import Control.Monad (replicateM, unless, when)
import Control.Monad.Extra (concatMapM)
import Data.Bool (bool)
import Data.Char (toUpper)
import Data.Default (Default (..))
import Data.Generics (everythingWithContext, everywhere, mkQ, mkT)
import qualified Data.Kind
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Typeable (Typeable)
import GHC.Stack
import GHC.TypeLits (Symbol)
import Language.Haskell.TH hiding (Match, match)
import Language.Haskell.TH.Syntax (Lift (lift))
import Test.HMock.Internal.Core
import Test.HMock.Internal.Predicates (Predicate (accept, showPredicate), eq)

-- | Custom options for deriving a 'Mockable' class.
newtype MockableOptions = MockableOptions
  { -- | Suffix to add to 'Action' and 'Matcher' names.  Defaults to @""@.
    mockSuffix :: String
  }

instance Default MockableOptions where
  def = MockableOptions {mockSuffix = ""}

-- | Define all instances necessary to use HMock with the given class.
--
-- @'makeMockable' ''MyClass@ is equivalent to both @'deriveMockable' ''MyClass@
-- and @'deriveForMockT' ''MyClass@.
makeMockable :: Name -> Q [Dec]
makeMockable = makeMockableType . conT

-- | Define all instances necessary to use HMock with the given constraint type,
-- which should be a class applied to zero or more type arguments.
--
-- @'makeMockableType' [t|MyType|]@ is equivalent to both
-- @'deriveMockableType' [t|MyType|]@ and @'deriveTypeForMockT' [t|MyType|]@.
makeMockableType :: Q Type -> Q [Dec]
makeMockableType = makeMockableTypeWithOptions def

-- | Define all instances necessary to use HMock with the given class.  This is
-- like 'makeMockable', but with the ability to specify custom options.
makeMockableWithOptions :: MockableOptions -> Name -> Q [Dec]
makeMockableWithOptions options = makeMockableTypeWithOptions options . conT

-- | Define all instances necessary to use HMock with the given constraint type,
-- which should be a class applied to zero or more type arguments.  This is
-- like 'makeMockableType', but with the ability to specify custom options.
makeMockableTypeWithOptions :: MockableOptions -> Q Type -> Q [Dec]
makeMockableTypeWithOptions options qt =
  (++) <$> deriveMockableTypeWithOptions options qt
    <*> deriveTypeForMockTWithOptions options qt

-- | Defines the 'Mockable' instance for the given class.  This includes the
-- 'Action' and 'Matcher' types, and its exact 'Matcher's.
deriveMockable :: Name -> Q [Dec]
deriveMockable = deriveMockableType . conT

-- | Defines the 'Mockable' instance for the given constraint type, which should
-- be a class applied to zero or more type arguments.  This includes the
-- 'Action' and 'Matcher' types, and its exact 'Matcher's.
deriveMockableType :: Q Type -> Q [Dec]
deriveMockableType = deriveMockableTypeWithOptions def

-- | Defines the 'Mockable' instance for the given class.  This includes the
-- 'Action' and 'Matcher' types, and its exact 'Matcher's.  This is like
-- 'deriveMockable', but with the ability to specify custom options.
deriveMockableWithOptions :: MockableOptions -> Name -> Q [Dec]
deriveMockableWithOptions options = deriveMockableTypeWithOptions options . conT

-- | Defines the 'Mockable' instance for the given constraint type, which should
-- be a class applied to zero or more type arguments.  This includes the
-- 'Action' and 'Matcher' types, and its exact 'Matcher's.  This is like
-- 'deriveMockableType', but with the ability to specify custom options.
deriveMockableTypeWithOptions :: MockableOptions -> Q Type -> Q [Dec]
deriveMockableTypeWithOptions = deriveMockableImpl

-- | Defines an instance of the given class for 'MockT', delegating all of its
-- methods to 'mockMethod' to be handled by HMock.
deriveForMockT :: Name -> Q [Dec]
deriveForMockT = deriveTypeForMockT . conT

-- | Defines an instance of the given constraint type for 'MockT', delegating
-- all of its methods to 'mockMethod' to be handled by HMock.  The type should
-- be a class applied to zero or more type arguments.
deriveTypeForMockT :: Q Type -> Q [Dec]
deriveTypeForMockT = deriveTypeForMockTWithOptions def

-- | Defines an instance of the given class for 'MockT', delegating all of its
-- methods to 'mockMethod' to be handled by HMock.  This is like
-- 'deriveForMockT', but with the ability to specify custom options.
deriveForMockTWithOptions :: MockableOptions -> Name -> Q [Dec]
deriveForMockTWithOptions options = deriveTypeForMockTWithOptions options . conT

-- | Defines an instance of the given constraint type for 'MockT', delegating
-- all of its methods to 'mockMethod' to be handled by HMock.  The type should
-- be a class applied to zero or more type arguments.  This is like
-- 'deriveTypeForMockT', but with the ability to specify custom options.
deriveTypeForMockTWithOptions :: MockableOptions -> Q Type -> Q [Dec]
deriveTypeForMockTWithOptions = deriveForMockTImpl

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

data Instance = Instance
  { instanceType :: Type,
    instanceRequiredContext :: Cxt,
    exactParams :: [(Name, Type)],
    generalizedParams :: [Name]
  }
  deriving (Show)

monadVar :: Instance -> Name
monadVar inst = last (generalizedParams inst)

internalError :: HasCallStack => Q a
internalError = error "Internal error in HMock.  Please report this as a bug."

getInstance :: Type -> Q Instance
getInstance ty = withClass ty go
  where
    go (ClassD _ className [] _ _) =
      fail $ "Class " ++ nameBase className ++ " has no type parameters."
    go (ClassD cx _ params _ _) = matchVars ty [] (tvName <$> params)
      where
        matchVars :: Type -> [Type] -> [Name] -> Q Instance
        matchVars (AppT _ _) _ [_] =
          fail $ pprint ty ++ " is applied to too many arguments."
        matchVars (AppT a b) ts (_ : ps) =
          checkExts [FlexibleInstances] >> matchVars a (b : ts) ps
        matchVars _ ts ps =
          return
            ( Instance
                (foldl' (\t' v -> AppT t' (VarT v)) ty (init ps))
                (substTypeVars tbl <$> cx)
                tbl
                ps
            )
          where
            tbl = zip (tvName <$> params) ts
    go _ = internalError

substTypeVars :: [(Name, Type)] -> Type -> Type
substTypeVars classVars = everywhere (mkT subst)
  where
    subst (VarT x) | Just t <- lookup x classVars = t
    subst t = t

getMembers :: Type -> Q [Dec]
getMembers t = withClass t go
  where
    go (ClassD _ _ _ _ members) = return members
    go _ = internalError

data Method = Method
  { methodName :: Name,
    methodTyVars :: [TyVarBndr],
    methodCxt :: Cxt,
    methodArgs :: [Type],
    methodResult :: Type
  }

getMethods :: Type -> Q [Method]
getMethods t =
  filter knownResult
    <$> (mapMaybe . parseMethod <$> getInstance t <*> getMembers t)
  where
    knownResult method = isKnownType method (methodResult method)

parseMethod :: Instance -> Dec -> Maybe Method
parseMethod inst (SigD name ty)
  | (tvs, cx, argsAndReturn) <-
      splitType (substTypeVars (exactParams inst) ty),
    AppT (VarT _) result <- last argsAndReturn =
    Just (Method name tvs cx (map replaceMonadVar (init argsAndReturn)) result)
  where
    replaceMonadVar =
      substTypeVars
        [(monadVar inst, AppT (ConT ''MockT) (VarT (monadVar inst)))]
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

varsToConstraints :: TypeQ -> [Name] -> CxtQ
varsToConstraints ty = traverse (appT ty . varT)

deriveMockableImpl :: MockableOptions -> Q Type -> Q [Dec]
deriveMockableImpl options qt = do
  checkExts [GADTs, TypeFamilies, DataKinds]

  t <- qt
  inst <- getInstance t

  methods <- getMethods t
  when (null methods) $ do
    fail $
      "Cannot derive Mockable because " ++ pprint t
        ++ " has no mockable methods."

  let m = last (generalizedParams inst)
  (++)
    <$> sequenceA
      [ instanceD
          (varsToConstraints (conT ''Typeable) (init (generalizedParams inst)))
          [t|Mockable $(pure (instanceType inst))|]
          [ defineActionType options (instanceType inst) m methods,
            defineMatcherType options (instanceType inst) m methods,
            defineShowAction options methods,
            defineShowMatcher options methods,
            defineMatch options methods
          ]
      ]
    <*> defineExactMatchers options (instanceType inst) m methods

defineActionType :: MockableOptions -> Type -> Name -> [Method] -> DecQ
defineActionType options t m methods = do
  kind <-
    [t|
      Symbol ->
      (Data.Kind.Type -> Data.Kind.Type) ->
      Data.Kind.Type ->
      Data.Kind.Type
      |]
  let cons = actionConstructor options t m <$> methods
  dataInstD (pure []) ''Action [pure t] (Just kind) cons []

actionConstructor :: MockableOptions -> Type -> Name -> Method -> ConQ
actionConstructor options t m method = do
  forallC [] (return (methodCxt method)) $
    gadtC
      [getActionName options method]
      [ return (Bang NoSourceUnpackedness NoSourceStrictness, argTy)
        | argTy <- methodArgs method
      ]
      [t|
        Action
          $(pure t)
          $(litT (strTyLit (nameBase (methodName method))))
          $(varT m)
          $(pure (methodResult method))
        |]

getActionName :: MockableOptions -> Method -> Name
getActionName options method =
  mkName (map toUpper (take 1 name) ++ drop 1 name ++ mockSuffix options)
  where
    name = nameBase (methodName method)

defineMatcherType :: MockableOptions -> Type -> Name -> [Method] -> Q Dec
defineMatcherType options t m methods = do
  kind <-
    [t|
      Symbol ->
      (Data.Kind.Type -> Data.Kind.Type) ->
      Data.Kind.Type ->
      Data.Kind.Type
      |]
  let cons = matcherConstructor options t m <$> methods
  dataInstD (pure []) ''Matcher [pure t] (Just kind) cons []

matcherConstructor :: MockableOptions -> Type -> Name -> Method -> ConQ
matcherConstructor options t m method = do
  gadtC
    [getMatcherName options method]
    [ (Bang NoSourceUnpackedness NoSourceStrictness,) <$> mkPredicate argTy
      | argTy <- methodArgs method
    ]
    [t|
      Matcher
        $(pure t)
        $(litT (strTyLit (nameBase (methodName method))))
        $(varT m)
        $(pure (methodResult method))
      |]
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
getMatcherName options method =
  mkName (map toUpper (take 1 name) ++ drop 1 name ++ mockSuffix options ++ "_")
  where
    name = nameBase (methodName method)

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
    [ conP
        (getActionName options method)
        (zipWith argPattern (methodArgs method) argVars)
    ]
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
    canShow ty
      | not (null (freeTypeVars ty)) = return False
      | otherwise = isInstance ''Show [ty]
    argPattern ty v = canShow ty >>= bool wildP (varP v)
    showArg ty var =
      canShow ty
        >>= bool
          (lift ("(_ :: " ++ pprint ty ++ ")"))
          [|showsPrec 11 $(varE var) ""|]

defineShowMatcher :: MockableOptions -> [Method] -> Q Dec
defineShowMatcher options methods = do
  clauses <- concatMapM (showMatcherClauses options) methods
  funD 'showMatcher clauses

showMatcherClauses :: MockableOptions -> Method -> Q [ClauseQ]
showMatcherClauses options method = do
  argTVars <- replicateM (length (methodArgs method)) (newName "t")
  predVars <- replicateM (length (methodArgs method)) (newName "p")
  let actionArgs = zipWith actionArg argTVars (methodArgs method)
  let matcherArgs = varP <$> predVars
  let printedArgs = zipWith3 printedArg predVars argTVars (methodArgs method)
  let polyMatcherArgs = zipWith matcherArg predVars (methodArgs method)
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
          conP (getMatcherName options method) polyMatcherArgs
        ]
        (body (nameBase (methodName method)) printedPolyArgs)
        []
    ]
  where
    actionArg t ty
      | isKnownType method ty = wildP
      | otherwise = checkExts [ScopedTypeVariables] >> sigP wildP (varT t)

    matcherArg p ty
      | isKnownType method ty = varP p
      | otherwise = wildP

    printedArg p t ty
      | isKnownType method ty = [|"«" ++ showPredicate $(varE p) ++ "»"|]
      | otherwise =
        [|"«" ++ showPredicate ($(varE p) :: Predicate $(varT t)) ++ "»"|]

    printedPolyArg p ty
      | isKnownType method ty = [|"«" ++ showPredicate $(varE p) ++ "»"|]
      | otherwise = [|"«polymorphic»"|]

defineMatch :: MockableOptions -> [Method] -> Q Dec
defineMatch options methods = funD 'match (matchClause options <$> methods)

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
        [ (,) <$> normalG [|$(varE mmVar) == 0|] <*> [|Match Refl|],
          (,) <$> normalG [|otherwise|] <*> [|NoMatch $(varE mmVar)|]
        ]
    )
    [ valD
        (varP mmVar)
        (normalB [|length (filter not $(listE (mkAccept <$> argVars)))|])
        []
    ]
  where
    mkAccept (p, a) = [|accept $(return (VarE p)) $(return (VarE a))|]

defineExactMatchers :: MockableOptions -> Type -> Name -> [Method] -> Q [Dec]
defineExactMatchers options t m = concatMapM (defineExactMatcher options t m)

getExactMatcherName :: MockableOptions -> Method -> Name
getExactMatcherName options name =
  mkName (nameBase (methodName name) ++ mockSuffix options ++ "_")

defineExactMatcher :: MockableOptions -> Type -> Name -> Method -> Q [Dec]
defineExactMatcher options t m method = do
  maybeCxt <- wholeCxt (methodArgs method)
  case maybeCxt of
    Just cx -> do
      argVars <- replicateM (length (methodArgs method)) (newName "a")
      sequenceA
        [ sigD
            (getExactMatcherName options method)
            ( forallT
                []
                (pure (methodCxt method ++ cx))
                ( foldr
                    (\argTy ty -> [t|$(pure argTy) -> $ty|])
                    [t|
                      Matcher
                        $(pure t)
                        $(litT (strTyLit (nameBase (methodName method))))
                        $(varT m)
                        $(pure (methodResult method))
                      |]
                    (methodArgs method)
                )
            ),
          funD
            (getExactMatcherName options method)
            [ clause
                [varP v | v <- argVars]
                ( normalB
                    (makeBody argVars (conE (getMatcherName options method)))
                )
                []
            ]
        ]
    _ -> pure []
  where
    makeBody [] e = e
    makeBody (v : vs) e = makeBody vs [|$e (eq $(varE v))|]

    wholeCxt :: [Type] -> Q (Maybe Cxt)
    wholeCxt (ty : ts) = do
      thisCxt <- argCxt ty
      otherCxt <- wholeCxt ts
      return ((++) <$> thisCxt <*> otherCxt)
    wholeCxt [] = return (Just [])

    argCxt :: Type -> Q (Maybe Cxt)
    argCxt argTy
      | not (isKnownType method argTy) = return Nothing
      | VarT v <- argTy =
        Just <$> sequenceA [[t|Eq $(varT v)|], [t|Show $(varT v)|]]
      | otherwise = do
        eqInstances <- reifyInstances ''Eq [argTy]
        showInstances <- reifyInstances ''Show [argTy]
        case (eqInstances, showInstances) of
          ([InstanceD _ eqCxt _ _], [InstanceD _ showCxt _ _]) ->
            return (Just (filterCxt argTy eqCxt ++ filterCxt argTy showCxt))
          _ -> return Nothing

    filterCxt :: Type -> Cxt -> Cxt
    filterCxt ty = filter (all (`elem` freeTypeVars ty) . freeTypeVars)

isKnownType :: Method -> Type -> Bool
isKnownType method argTy = null tyVars && null cx
  where
    (tyVars, cx) =
      relevantContext argTy (methodTyVars method, methodCxt method)

deriveForMockTImpl :: MockableOptions -> Q Type -> Q [Dec]
deriveForMockTImpl options qt = do
  t <- qt
  inst <- getInstance t

  members <- getMembers t
  methods <- getMethods t
  when (length methods < length members) $
    fail $
      "Cannot derive MockT because " ++ pprint t ++ " has unmockable methods."

  m <- newName "m"
  let decs = map (mockMethodImpl options) methods

  sequenceA
    [ instanceD
        ( concat
            <$> sequenceA
              [ varsToConstraints
                  (conT ''Typeable)
                  (init (generalizedParams inst)),
                sequenceA
                  [ [t|Typeable $(varT m)|],
                    [t|Monad $(varT m)|]
                  ]
              ]
        )
        [t|$(pure (instanceType inst)) (MockT $(varT m))|]
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
