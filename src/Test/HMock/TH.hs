{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides Template Haskell splices that can be used to derive
-- boilerplate instances for HMock.  'makeMockable' implements the common case
-- where you just want to generate everything you need to mock with a class.
-- The variant 'makeMockableWithOptions' is similar, but takes an options
-- parameter that can be used to customize the generation.
module Test.HMock.TH
  ( MakeMockableOptions (..),
    makeMockable,
    makeMockableWithOptions,
  )
where

import Control.Monad (replicateM, unless, when, zipWithM)
import Control.Monad.Extra (concatMapM)
import Control.Monad.Trans (MonadIO)
import Data.Bool (bool)
import Data.Char (toUpper)
import Data.Default (Default (..))
import Data.Either (partitionEithers)
import qualified Data.Kind
import Data.List (foldl', (\\))
import Data.Maybe (catMaybes, isNothing)
import Data.Proxy (Proxy)
import Data.Typeable (Typeable, typeRep)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (ErrorMessage (Text, (:$$:), (:<>:)), Symbol, TypeError)
import Language.Haskell.TH hiding (Match, match)
import Language.Haskell.TH.Syntax (Lift (lift))
import Test.HMock.Internal.State (MockT)
import Test.HMock.Internal.TH
import Test.HMock.MockMethod (mockDefaultlessMethod, mockMethod)
import Test.HMock.Mockable (MatchResult (..), Mockable, MockableBase (..))
import Test.HMock.Rule (Expectable (..))
import Test.Predicates (Predicate (..), eq)

-- | Custom options for deriving 'MockableBase' and related instances.
data MakeMockableOptions = MakeMockableOptions
  { -- | Whether to generate a 'Mockable' instance with an empty setup.
    -- Defaults to 'True'.
    --
    -- If this is 'False', you are responsible for providing a 'Mockable'
    -- instance as follows:
    --
    -- @
    -- instance 'Mockable' MyClass where
    --   'Test.HMock.Mockable.setupMockable' _ = ...
    -- @
    mockEmptySetup :: Bool,
    -- | Whether to derive instances of the class for 'MockT' or not.  Defaults
    -- to 'True'.
    --
    -- This option will cause a build error if some members of the class are
    -- unmockable or are not methods.  In this case, you'll need to define this
    -- instance yourself, delegating the mockable methods as follows:
    --
    -- @
    -- instance MyClass ('MockT' m) where
    --   myMethod x y = 'mockMethod' (MyMethod x y)
    --   ...
    -- @
    mockDeriveForMockT :: Bool,
    -- | Suffix to add to 'Action' and 'Matcher' names.  Defaults to @""@.
    mockSuffix :: String,
    -- | Whether to warn about limitations of the generated mocks.  This is
    -- mostly useful temporarily for finding out why generated code doesn't
    -- match your expectations.  Defaults to @'False'@.
    mockVerbose :: Bool
  }

instance Default MakeMockableOptions where
  def =
    MakeMockableOptions
      { mockEmptySetup = True,
        mockDeriveForMockT = True,
        mockSuffix = "",
        mockVerbose = False
      }

-- | Defines all instances necessary to use HMock with the given type, using
-- default options.  The type should be a type class extending 'Monad', applied
-- to zero or more type arguments.
--
-- This defines all of the following instances, if necessary:
--
-- * 'MockableBase' and the associated 'Action' and 'Matcher' types.
-- * 'Expectable' instances for the 'Action' type.
-- * 'Mockable' with an empty setup.
-- * Instances of the provided application type class to allow unit tests to be
--   run with the 'MockT' monad transformer.
makeMockable :: Q Type -> Q [Dec]
makeMockable qtype = makeMockableWithOptions qtype def

-- | Defines all instances necessary to use HMock with the given type, using
-- the provided options.  The type should be a type class extending 'Monad',
-- applied to zero or more type arguments.
--
-- This defines the following instances, if necessary:
--
-- * 'MockableBase' and the associated 'Action' and 'Matcher' types.
-- * 'Expectable' instances for the 'Action' type.
-- * If 'mockEmptySetup' is 'True': 'Mockable' with an empty setup.
-- * If 'mockDeriveForMockT' is 'True': Instances of the provided application
--   type class to allow unit tests to be run with the 'MockT' monad
--   transformer.
makeMockableWithOptions :: Q Type -> MakeMockableOptions -> Q [Dec]
makeMockableWithOptions qtype options = makeMockableImpl options qtype

data Instance = Instance
  { instType :: Type,
    instRequiredContext :: Cxt,
    instGeneralParams :: [Name],
    instMonadVar :: Name,
    instMethods :: [Method],
    instExtraMembers :: [Dec]
  }
  deriving (Show)

data Method = Method
  { methodName :: Name,
    methodTyVars :: [Name],
    methodCxt :: Cxt,
    methodArgs :: [Type],
    methodResult :: Type
  }
  deriving (Show)

withClass :: Type -> (Dec -> Q a) -> Q a
withClass t f = do
  case unappliedName t of
    Just cls -> do
      info <- reify cls
      case info of
        ClassI dec@ClassD {} _ -> f dec
        _ -> fail $ "Expected " ++ show cls ++ " to be a class, but it wasn't."
    _ -> fail "Expected a class, but got something else."

getInstance :: MakeMockableOptions -> Type -> Q Instance
getInstance options ty = withClass ty go
  where
    go (ClassD _ className [] _ _) =
      fail $ "Class " ++ nameBase className ++ " has no type parameters."
    go (ClassD cx _ params _ members) =
      matchVars ty [] (tvName <$> params)
      where
        matchVars :: Type -> [Type] -> [Name] -> Q Instance
        matchVars _ _ [] = internalError
        matchVars (AppT _ _) _ [_] =
          fail $ pprint ty ++ " is applied to too many arguments."
        matchVars (AppT a b) ts (_ : ps) =
          checkExt FlexibleInstances >> matchVars a (b : ts) ps
        matchVars _ ts ps = do
          let genVars = init ps
          let mVar = last ps
          let t = foldl' (\t' v -> AppT t' (VarT v)) ty genVars
          let tbl = zip (tvName <$> params) ts
          let cx' = substTypeVars tbl <$> cx
          makeInstance options t cx' tbl genVars mVar members
    go _ = internalError

makeInstance ::
  MakeMockableOptions ->
  Type ->
  Cxt ->
  [(Name, Type)] ->
  [Name] ->
  Name ->
  [Dec] ->
  Q Instance
makeInstance options ty cx tbl ps m members = do
  processedMembers <- mapM (getMethod ty m tbl) $ filter isRelevantMember members
  (extraMembers, methods) <-
    partitionEithers <$> zipWithM memberOrMethod members processedMembers
  return $
    Instance
      { instType = ty,
        instRequiredContext = cx,
        instGeneralParams = ps,
        instMonadVar = m,
        instMethods = methods,
        instExtraMembers = extraMembers
      }
  where
    isRelevantMember :: Dec -> Bool
    isRelevantMember DefaultSigD {} = False
    isRelevantMember _ = True

    memberOrMethod :: Dec -> Either [String] Method -> Q (Either Dec Method)
    memberOrMethod dec (Left warnings) = do
      when (mockVerbose options) $ mapM_ reportWarning warnings
      return (Left dec)
    memberOrMethod _ (Right method) = return (Right method)

getMethod :: Type -> Name -> [(Name, Type)] -> Dec -> Q (Either [String] Method)
getMethod instTy m tbl (SigD name ty) = do
  simpleTy <- localizeMember instTy m (substTypeVars tbl ty)
  let (tvs, cx, args, mretval) = splitType simpleTy
  return $ do
    retval <- case mretval of
      AppT (VarT m') retval | m' == m -> return retval
      _ ->
        Left
          [ nameBase name
              ++ " can't be mocked: return value not in the expected monad."
          ]
    unless
      ( all
          (isVarTypeable cx)
          (filter (`elem` tvs) (freeTypeVars retval))
      )
      $ Left
        [ nameBase name
            ++ " can't be mocked: return value not Typeable."
        ]
    let argTypes = map (substTypeVar m (AppT (ConT ''MockT) (VarT m))) args
    when (any hasNestedPolyType argTypes) $
      Left
        [ nameBase name
            ++ " can't be mocked: rank-n types nested in arguments."
        ]

    return $
      Method
        { methodName = name,
          methodTyVars = tvs,
          methodCxt = cx,
          methodArgs = argTypes,
          methodResult = retval
        }
  where
    isVarTypeable :: Cxt -> Name -> Bool
    isVarTypeable cx v = AppT (ConT ''Typeable) (VarT v) `elem` cx
getMethod _ _ _ (DataD _ name _ _ _ _) =
  return $
    Left [nameBase name ++ " must be defined manually in MockT instance."]
getMethod _ _ _ (NewtypeD _ name _ _ _ _) =
  return $
    Left [nameBase name ++ " must be defined manually in MockT instance."]
getMethod _ _ _ (TySynD name _ _) =
  return $
    Left [nameBase name ++ " must be defined manually in MockT instance."]
getMethod _ _ _ (DataFamilyD name _ _) =
  return $
    Left [nameBase name ++ " must be defined manually in MockT instance."]
getMethod _ _ _ (OpenTypeFamilyD (TypeFamilyHead name _ _ _)) =
  return $
    Left [nameBase name ++ " must be defined manually in MockT instance."]
getMethod _ _ _ (ClosedTypeFamilyD (TypeFamilyHead name _ _ _) _) =
  return $
    Left [nameBase name ++ " must be defined manually in MockT instance."]
getMethod _ _ _ _ = return (Left [])

isKnownType :: Method -> Type -> Bool
isKnownType method ty = null tyVars && null cx
  where
    (tyVars, cx) =
      relevantContext ty (methodTyVars method, methodCxt method)

withMethodParams :: Instance -> Method -> TypeQ -> TypeQ
withMethodParams inst method t =
  [t|
    $t
      $(pure (instType inst))
      $(litT (strTyLit (nameBase (methodName method))))
      $(varT (instMonadVar inst))
      $(pure (methodResult method))
    |]

makeMockableImpl :: MakeMockableOptions -> Q Type -> Q [Dec]
makeMockableImpl options qtype = do
  checkExt DataKinds
  checkExt FlexibleInstances
  checkExt GADTs
  checkExt MultiParamTypeClasses
  checkExt ScopedTypeVariables
  checkExt TypeFamilies

  ty <- qtype
  let generalizedTy = case unappliedName ty of
        Just cls -> ConT cls
        _ -> ty
  inst <- getInstance options generalizedTy

  when (null (instMethods inst)) $ do
    fail $
      "Cannot derive Mockable because " ++ pprint (instType inst)
        ++ " has no mockable methods."

  typeableCxt <- constrainVars [conT ''Typeable] (instGeneralParams inst)

  needsMockableBase <-
    isNothing <$> resolveInstance ''MockableBase [instType inst]
  mockableBase <-
    if needsMockableBase
      then do
        mockableBase <-
          instanceD
            (pure typeableCxt)
            [t|MockableBase $(pure (instType inst))|]
            [ defineActionType options inst,
              defineMatcherType options inst,
              defineShowAction options (instMethods inst),
              defineShowMatcher options (instMethods inst),
              defineMatchAction options (instMethods inst)
            ]
        expectables <- defineExpectableActions options inst
        return (mockableBase : expectables)
      else return []

  needsMockable <-
    if mockEmptySetup options
      then isNothing <$> resolveInstance ''Mockable [instType inst]
      else return False
  mockable <-
    if needsMockable
      then do
        t <- [t|Mockable $(pure (instType inst))|]
        return [InstanceD (Just Overlappable) typeableCxt t []]
      else return []

  mockt <- deriveForMockT options ty

  return $ mockableBase ++ mockable ++ mockt

defineActionType :: MakeMockableOptions -> Instance -> DecQ
defineActionType options inst = do
  kind <-
    [t|
      Symbol ->
      (Data.Kind.Type -> Data.Kind.Type) ->
      Data.Kind.Type ->
      Data.Kind.Type
      |]
  let cons = actionConstructor options inst <$> instMethods inst
  dataInstD
    (pure [])
    ''Action
    [pure (instType inst)]
    (Just kind)
    cons
    []

actionConstructor :: MakeMockableOptions -> Instance -> Method -> ConQ
actionConstructor options inst method = do
  forallC [] (return (methodCxt method)) $
    gadtC
      [getActionName options method]
      [ return (Bang NoSourceUnpackedness NoSourceStrictness, argTy)
        | argTy <- methodArgs method
      ]
      (withMethodParams inst method [t|Action|])

getActionName :: MakeMockableOptions -> Method -> Name
getActionName options method =
  mkName (map toUpper (take 1 name) ++ drop 1 name ++ mockSuffix options)
  where
    name = nameBase (methodName method)

defineMatcherType :: MakeMockableOptions -> Instance -> Q Dec
defineMatcherType options inst = do
  kind <-
    [t|
      Symbol ->
      (Data.Kind.Type -> Data.Kind.Type) ->
      Data.Kind.Type ->
      Data.Kind.Type
      |]
  let cons = matcherConstructor options inst <$> instMethods inst
  dataInstD
    (pure [])
    ''Matcher
    [pure (instType inst)]
    (Just kind)
    cons
    []

matcherConstructor :: MakeMockableOptions -> Instance -> Method -> ConQ
matcherConstructor options inst method = do
  gadtC
    [getMatcherName options method]
    [ (Bang NoSourceUnpackedness NoSourceStrictness,) <$> mkPredicate argTy
      | argTy <- methodArgs method
    ]
    (withMethodParams inst method [t|Matcher|])
  where
    mkPredicate argTy
      | hasPolyType argTy = do
        checkExt RankNTypes
        v <- newName "t"
        forallT [bindVar v] (pure []) [t|Predicate $(varT v)|]
      | null tyVars && null cx = [t|Predicate $(pure argTy)|]
      | otherwise = do
        checkExt RankNTypes
        forallT (bindVar <$> tyVars) (pure cx) [t|Predicate $(pure argTy)|]
      where
        (tyVars, cx) =
          relevantContext argTy (methodTyVars method, methodCxt method)

getMatcherName :: MakeMockableOptions -> Method -> Name
getMatcherName options method =
  mkName (map toUpper (take 1 name) ++ drop 1 name ++ mockSuffix options ++ "_")
  where
    name = nameBase (methodName method)

defineShowAction :: MakeMockableOptions -> [Method] -> Q Dec
defineShowAction options methods =
  funD 'showAction (showActionClause options <$> methods)

showActionClause :: MakeMockableOptions -> Method -> Q Clause
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
    isLocalPoly ty =
      not . null . fst $
        relevantContext ty (methodTyVars method, methodCxt method)

    canShow ty
      | hasPolyType ty = return False
      | isLocalPoly ty = (`elem` methodCxt method) <$> [t|Show $(pure ty)|]
      | null (freeTypeVars ty) = isInstance ''Show [ty]
      | otherwise = return False

    canType ty
      | hasPolyType ty = return False
      | isLocalPoly ty =
        (`elem` methodCxt method)
          <$> [t|Typeable $(pure ty)|]
      | otherwise = return (null (freeTypeVars ty))

    argPattern ty v = canShow ty >>= flip sigP (pure ty) . bool wildP (varP v)

    showArg ty var = do
      showable <- canShow ty
      typeable <- canType ty
      case (showable, typeable) of
        (True, _) -> [|showsPrec 11 $(varE var) ""|]
        (_, True) ->
          [|
            "(_ :: "
              ++ show (typeRep (undefined :: Proxy $(return ty)))
              ++ ")"
            |]
        _ -> lift ("(_  :: " ++ pprint (removeModNames ty) ++ ")")

defineShowMatcher :: MakeMockableOptions -> [Method] -> Q Dec
defineShowMatcher options methods = do
  clauses <- concatMapM (showMatcherClauses options) methods
  funD 'showMatcher clauses

showMatcherClauses :: MakeMockableOptions -> Method -> Q [ClauseQ]
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
      | otherwise = sigP wildP (varT t)

    matcherArg p ty
      | isKnownType method ty = varP p
      | otherwise = wildP

    printedArg p t ty
      | isKnownType method ty = [|"«" ++ show $(varE p) ++ "»"|]
      | otherwise =
        [|"«" ++ show ($(varE p) :: Predicate $(varT t)) ++ "»"|]

    printedPolyArg p ty
      | isKnownType method ty = [|"«" ++ show $(varE p) ++ "»"|]
      | otherwise = [|"«polymorphic»"|]

defineMatchAction :: MakeMockableOptions -> [Method] -> Q Dec
defineMatchAction options methods =
  funD 'matchAction (matchActionClause options <$> methods)

matchActionClause :: MakeMockableOptions -> Method -> Q Clause
matchActionClause options method = do
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
        [ (,) <$> normalG [|null $(varE mmVar)|] <*> [|Match|],
          (,) <$> normalG [|otherwise|] <*> [|NoMatch $(varE mmVar)|]
        ]
    )
    [ valD
        (varP mmVar)
        ( normalB
            [|
              catMaybes $
                zipWith
                  (fmap . (,))
                  [1 ..]
                  $(listE (mkAccept <$> argVars))
              |]
        )
        []
    ]
  where
    mkAccept (p, a) =
      [|
        if accept $(return (VarE p)) $(return (VarE a))
          then Nothing
          else Just $ explain $(return (VarE p)) $(return (VarE a))
        |]

defineExpectableActions :: MakeMockableOptions -> Instance -> Q [Dec]
defineExpectableActions options inst =
  mapM (defineExpectableAction options inst) (instMethods inst)

type ComplexExpectableMessage name =
  ( 'Text "Method " ':<>: 'Text name
      ':<>: 'Text " is too complex to expect with an Action."
  )
    ':$$: 'Text "Suggested fix: Use a Matcher instead of an Action."

defineExpectableAction :: MakeMockableOptions -> Instance -> Method -> Q Dec
defineExpectableAction options inst method = do
  maybeCxt <- wholeCxt (methodArgs method)
  argVars <- replicateM (length (methodArgs method)) (newName "a")
  case maybeCxt of
    Just cx -> do
      instanceD
        (pure (methodCxt method ++ cx))
        ( appT
            (withMethodParams inst method [t|Expectable|])
            (withMethodParams inst method [t|Action|])
        )
        [ funD
            'toRule
            [ clause
                [conP (getActionName options method) (map varP argVars)]
                ( normalB $
                    let matcherCon = conE (getMatcherName options method)
                     in appE (varE 'toRule) (makeBody argVars matcherCon)
                )
                []
            ]
        ]
    _ -> do
      checkExt UndecidableInstances
      instanceD
        ( (: [])
            <$> [t|
              TypeError
                ( ComplexExpectableMessage
                    $(litT $ strTyLit $ nameBase $ methodName method)
                )
              |]
        )
        ( appT
            (withMethodParams inst method [t|Expectable|])
            (withMethodParams inst method [t|Action|])
        )
        [ funD
            'toRule
            [clause [] (normalB [|undefined|]) []]
        ]
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
      | otherwise =
        simplifyContext [AppT (ConT ''Eq) argTy, AppT (ConT ''Show) argTy]

deriveForMockT :: MakeMockableOptions -> Type -> Q [Dec]
deriveForMockT options ty = do
  inst <- getInstance options {mockVerbose = False} ty
  needsMockT <-
    if mockDeriveForMockT options
      then
        isNothing
          <$> resolveInstanceType
            ( AppT
                (instType inst)
                (AppT (ConT ''MockT) (VarT (instMonadVar inst)))
            )
      else return False

  if needsMockT
    then do
      unless (null (instExtraMembers inst)) $
        fail $
          "Cannot derive MockT because " ++ pprint (instType inst)
            ++ " has unmockable methods."

      m <- newName "m"
      let decs = map (implementMethod options) (instMethods inst)

      let cx =
            instRequiredContext inst
              \\ [ AppT (ConT ''Typeable) (VarT (instMonadVar inst)),
                   AppT (ConT ''Functor) (VarT (instMonadVar inst)),
                   AppT (ConT ''Applicative) (VarT (instMonadVar inst)),
                   AppT (ConT ''Monad) (VarT (instMonadVar inst)),
                   AppT (ConT ''MonadIO) (VarT (instMonadVar inst))
                 ]

      let mockTConstraints =
            substTypeVar
              (instMonadVar inst)
              (AppT (ConT ''MockT) (VarT m))
              <$> cx
      simplifyContext mockTConstraints
        >>= \case
          Just cxMockT ->
            (: [])
              <$> instanceD
                ( concat
                    <$> sequence
                      [ return cxMockT,
                        constrainVars [[t|Typeable|]] (instGeneralParams inst),
                        constrainVars [[t|Typeable|], [t|MonadIO|]] [m]
                      ]
                )
                [t|$(pure (instType inst)) (MockT $(varT m))|]
                decs
          Nothing -> fail "Missing MockT instance for a superclass."
    else return []

implementMethod :: MakeMockableOptions -> Method -> Q Dec
implementMethod options method = do
  argVars <- replicateM (length (methodArgs method)) (newName "a")
  funD
    (methodName method)
    [clause (varP <$> argVars) (normalB (body argVars)) []]
  where
    actionExp [] e = e
    actionExp (v : vs) e = actionExp vs [|$e $(varE v)|]

    body argVars = do
      defaultCxt <- simplifyContext [AppT (ConT ''Default) (methodResult method)]
      let someMockMethod = case defaultCxt of
            Just [] -> [|mockMethod|]
            _ -> [|mockDefaultlessMethod|]
      [|
        $someMockMethod
          $(actionExp argVars (conE (getActionName options method)))
        |]

checkExt :: Extension -> Q ()
checkExt e = do
  enabled <- isExtEnabled e
  unless enabled $
    fail $ "Please enable " ++ show e ++ " to generate this mock."

internalError :: HasCallStack => Q a
internalError = error "Internal error in HMock.  Please report this as a bug."
