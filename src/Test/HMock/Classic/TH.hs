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
--
-- To mock a [Bluefin](https://hackage.haskell.org/package/bluefin) capability
-- rather than an mtl-style type class, see "Test.HMock.Bluefin.TH".
module Test.HMock.Classic.TH
  ( MakeMockableOptions (..),
    makeMockable,
    makeMockableWithOptions,
  )
where

import Control.Monad (replicateM, unless, when, zipWithM)
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Trans (MonadIO)
import Data.Default (Default (..))
import Data.Either (partitionEithers)
import Data.List ((\\))
#if !MIN_VERSION_base(4, 20, 0)
import Data.List (foldl')
#endif
import Data.Maybe (isNothing)
import Data.Typeable (Typeable)
import Language.Haskell.TH hiding (Match, match)
import Test.HMock.Internal.MakeMockable
import Test.HMock.Classic.MockMethod (mockDefaultlessMethod, mockMethod)
import Test.HMock.Classic.MockT (MockT, Mockable)
import Test.HMock.Internal.TH
import Test.HMock.Internal.Util (choices)

-- | Custom options for deriving 'Test.HMock.Classic.MockableBase' and related instances.
data MakeMockableOptions = MakeMockableOptions
  { -- | Whether to generate a 'Mockable' instance with an empty setup.
    -- Defaults to 'True'.
    --
    -- If this is 'False', you are responsible for providing a 'Mockable'
    -- instance as follows:
    --
    -- @
    -- instance 'Mockable' MyClass where
    --   'Test.HMock.Classic.MockT.setupMockable' _ = ...
    -- @
    mockEmptySetup :: Bool,
    -- | Whether to derive instances of the class for t'MockT' or not.  Defaults
    -- to 'True'.
    --
    -- This option will cause a build error if some members of the class are
    -- unmockable or are not methods.  In this case, you'll need to define this
    -- instance yourself, delegating the mockable methods as follows:
    --
    -- @
    -- instance MyClass (t'MockT' m) where
    --   myMethod x y = 'mockMethod' (MyMethod x y)
    --   ...
    -- @
    mockDeriveForMockT :: Bool,
    -- | Suffix to add to 'Test.HMock.Classic.Action' and 'Test.HMock.Classic.Matcher' names.  Defaults to @""@.
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

-- | The part of the options the shared generators act on.
generatorOptions :: MakeMockableOptions -> GeneratorOptions
generatorOptions options =
  GeneratorOptions
    { genSuffix = mockSuffix options,
      genStripSuffix = ""
    }

-- | A class reified for mocking: the shared description the generators need,
-- plus what only the t'MockT' instance needs.
data ClassInstance = ClassInstance
  { classInstance :: Instance,
    -- | The class applied to everything but its monad, which is the index
    -- for 'Mockable' and the thing the t'MockT' instance is for.
    classType :: Type,
    -- | The class's monad parameter.
    classMonadVar :: Name,
    -- | The class's own superclass context, after substitution.
    classContext :: Cxt,
    -- | Members that can't be mocked, which rule out deriving for t'MockT'.
    classExtraMembers :: [Dec]
  }

-- | Defines all instances necessary to use HMock with the given type, using
-- default options.  The type should be a type class extending 'Monad', applied
-- to zero or more type arguments.
--
-- This defines all of the following instances, if necessary:
--
-- * 'Test.HMock.Classic.MockableBase' and the associated 'Test.HMock.Classic.Action' and 'Test.HMock.Classic.Matcher' types.
-- * 'Test.HMock.Classic.ExactMatch' instances, so an 'Test.HMock.Classic.Action' can stand in for a 'Test.HMock.Classic.Matcher'.
-- * 'Mockable' with an empty setup.
-- * Instances of the provided application type class to allow unit tests to be
--   run with the t'MockT' monad transformer.
makeMockable :: Q Type -> Q [Dec]
makeMockable qtype = makeMockableWithOptions qtype def

-- | Defines all instances necessary to use HMock with the given type, using
-- the provided options.  The type should be a type class extending 'Monad',
-- applied to zero or more type arguments.
--
-- This defines the following instances, if necessary:
--
-- * 'Test.HMock.Classic.MockableBase' and the associated 'Test.HMock.Classic.Action' and 'Test.HMock.Classic.Matcher' types.
-- * 'Test.HMock.Classic.ExactMatch' instances, so an 'Test.HMock.Classic.Action' can stand in for a 'Test.HMock.Classic.Matcher'.
-- * If 'mockEmptySetup' is 'True': 'Mockable' with an empty setup.
-- * If 'mockDeriveForMockT' is 'True': Instances of the provided application
--   type class to allow unit tests to be run with the t'MockT' monad
--   transformer.
makeMockableWithOptions :: Q Type -> MakeMockableOptions -> Q [Dec]
makeMockableWithOptions qtype options = makeMockableImpl options qtype

withClass :: Type -> (Dec -> Q a) -> Q a
withClass t f = do
  case unappliedName t of
    Just cls -> do
      info <- reify cls
      case info of
        ClassI dec@ClassD {} _ -> f dec
        _ -> fail $ "Expected " ++ show cls ++ " to be a class, but it wasn't."
    _ -> fail "Expected a class, but got something else."

getInstance :: MakeMockableOptions -> Type -> Q ClassInstance
getInstance options ty = withClass ty go
  where
    go (ClassD _ className [] _ _) =
      fail $ "Class " ++ nameBase className ++ " has no type parameters."
    go (ClassD cx _ params _ members) =
      matchVars ty [] (tvName <$> params)
      where
        matchVars :: Type -> [Type] -> [Name] -> Q ClassInstance
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
  Q ClassInstance
makeInstance options ty cx tbl ps m members = do
  processedMembers <- mapM (getMethod ty m tbl) $ filter isRelevantMember members
  (extraMembers, methods) <-
    partitionEithers <$> zipWithM memberOrMethod members processedMembers
  return
    ClassInstance
      { classInstance =
          Instance
            { instType = AppT ty (VarT m),
              instGeneralParams = ps ++ [m],
              instMethods = methods
            },
        classType = ty,
        classMonadVar = m,
        classContext = cx,
        classExtraMembers = extraMembers
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

makeMockableImpl :: MakeMockableOptions -> Q Type -> Q [Dec]
makeMockableImpl options qtype = do
  checkMockableExtensions
  ty <- qtype
  let generalizedTy = case unappliedName ty of
        Just cls -> ConT cls
        _ -> ty
  ci <- getInstance options generalizedTy
  shared <- defineMockableInstances (generatorOptions options) (classInstance ci)
  mockable <- deriveMockable options ci
  mockt <- deriveForMockT options ty
  return (shared ++ mockable ++ mockt)

-- | Generates an empty 'Mockable' instance, if 'mockEmptySetup' asks for one
-- and there isn't one already.
deriveMockable :: MakeMockableOptions -> ClassInstance -> Q [Dec]
deriveMockable options ci
  | mockEmptySetup options = do
      needed <- isNothing <$> resolveInstance ''Mockable [classType ci]
      if needed
        then do
          cx <-
            constrainVars
              [conT ''Typeable]
              (instGeneralParams (classInstance ci) \\ [classMonadVar ci])
          t <- [t|Mockable $(pure (classType ci))|]
          return [InstanceD (Just Overlappable) cx t []]
        else return []
  | otherwise = return []

deriveForMockT :: MakeMockableOptions -> Type -> Q [Dec]
deriveForMockT options ty = do
  ci <- getInstance options {mockVerbose = False} ty
  let inst = classInstance ci
  needsMockT <-
    if mockDeriveForMockT options
      then
        isNothing
          <$> resolveInstanceType
            ( AppT
                (classType ci)
                (AppT (ConT ''MockT) (VarT (classMonadVar ci)))
            )
      else return False

  if needsMockT
    then do
      unless (null (classExtraMembers ci)) $
        fail $
          "Cannot derive MockT because " ++ pprint (instType inst)
            ++ " has unmockable methods."

      m <- newName "m"
      let decs = map (implementMethod (generatorOptions options)) (instMethods inst)

      let cx =
            classContext ci
              \\ [ AppT (ConT ''Typeable) (VarT (classMonadVar ci)),
                   AppT (ConT ''Functor) (VarT (classMonadVar ci)),
                   AppT (ConT ''Applicative) (VarT (classMonadVar ci)),
                   AppT (ConT ''Monad) (VarT (classMonadVar ci)),
                   AppT (ConT ''MonadIO) (VarT (classMonadVar ci))
                 ]

      let mockTConstraints =
            substTypeVar
              (classMonadVar ci)
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
                        constrainVars
                          [[t|Typeable|]]
                          (instGeneralParams inst \\ [classMonadVar ci]),
                        constrainVars [[t|Typeable|], [t|MonadIO|]] [m]
                      ]
                )
                [t|$(pure (classType ci)) (MockT $(varT m))|]
                decs
          Nothing -> fail "Missing MockT instance for a superclass."
    else return []

implementMethod :: GeneratorOptions -> Method -> Q Dec
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

-- | Substitutes a type for a single type variable.
substTypeVar :: Name -> Type -> Type -> Type
substTypeVar n t = substTypeVars [(n, t)]

-- | Remove instance context from a method.
--
-- Some GHC versions report class members including the instance context (for
-- example, @show :: Show a => a -> String@, instead of @show :: a -> String@).
-- This looks for the instance context, and substitutes if needed to eliminate
-- it.
localizeMember :: Type -> Name -> Type -> Q Type
localizeMember instTy m t@(ForallT tvs cx ty) = do
  let fullConstraint = AppT instTy (VarT m)
  let unifyLeft (c, cs) = fmap (,cs) <$> unifyTypes c fullConstraint
  results <- mapMaybeM unifyLeft (choices cx)
  case results of
    ((tbl, remainingCx) : _) -> do
      let cx' = substTypeVars tbl <$> remainingCx
          ty' = substTypeVars tbl ty
          (tvs', cx'') =
            relevantContext
              ty'
              ((tvName <$> tvs) \\ (fst <$> tbl), cx')
          t'
            | null tvs' && null cx'' = ty'
            | otherwise = ForallT (bindVar <$> tvs') cx'' ty'
      return t'
    _ -> return t
localizeMember _ _ t = return t
