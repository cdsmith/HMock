{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell splices for mocking [Bluefin](https://hackage.haskell.org/package/bluefin)
-- capabilities.
--
-- 'makeMockableCapability' takes a capability: a record of effectful
-- operations, in the style described by "Bluefin.Compound".  To mock an
-- mtl-style type class instead, see "Test.HMock.Classic.TH".
--
-- @
-- data FileSystem e = MkFileSystem
--   { readFileImpl :: FilePath -> 'Bluefin.Eff.Eff' e String,
--     writeFileImpl :: FilePath -> String -> Eff e ()
--   }
--
-- 'makeMockableCapability' [t|FileSystem|]
-- @
module Test.HMock.Bluefin.TH
  ( MakeMockableCapabilityOptions (..),
    makeMockableCapability,
    makeMockableCapabilityWithOptions,
  )
where

import Bluefin.Eff (Eff)
import Control.Monad (replicateM, unless, when)
import Data.Default (Default (..))
import Data.Either (partitionEithers)
import Data.List (intercalate)
#if !MIN_VERSION_base(4, 20, 0)
import Data.List (foldl')
#endif
import Data.Maybe (isJust, isNothing)
import Data.Typeable (Typeable)
import GHC.Stack (withFrozenCallStack)
import Language.Haskell.TH hiding (Match, match)
import Test.HMock.Bluefin.Mock
  ( MockableCapability (..),
    mockDefaultlessMethod,
    mockMethod,
    withUnlift,
  )
import Test.HMock.Internal.MakeMockable
import Test.HMock.Internal.TH

-- | Custom options for deriving the instances needed to mock a Bluefin
-- capability.
data MakeMockableCapabilityOptions = MakeMockableCapabilityOptions
  { -- | Whether to generate the 'MockableCapability' instance, which builds
    -- the capability record with its operations delegated to the mock.
    -- Defaults to 'True'.
    --
    -- This option will cause a build error if some of the capability's
    -- operations are unmockable.  In that case you'll need to define the
    -- instance yourself, delegating the mockable operations with
    -- 'Test.HMock.Bluefin.mockMethod':
    --
    -- @
    -- instance 'MockableCapability' MyCapability where
    --   'mocked' cap = MkMyCapability { myOp = \x -> 'Test.HMock.Bluefin.mockMethod' cap (MyOp x), ... }
    -- @
    capDeriveInstance :: Bool,
    -- | Suffix to add to 'Test.HMock.Bluefin.Action' and
    -- 'Test.HMock.Bluefin.Matcher' constructor names.  Defaults to @""@.
    capSuffix :: String,
    -- | Suffix to strip from a field name to get the name of the operation it
    -- implements.  Defaults to @"Impl"@.
    --
    -- Bluefin's convention, described in "Bluefin.Compound", is to name the
    -- record field @fooImpl@ so that the plain name @foo@ is free for the
    -- wrapper that callers actually use:
    --
    -- @
    -- data FileSystem e = MkFileSystem
    --   { readFileImpl :: FilePath -> Eff e String }
    --
    -- readFile :: (e <: es) => FileSystem e -> FilePath -> Eff es String
    -- readFile fs path = readFileImpl ('Bluefin.Compound.mapHandle' fs) path
    -- @
    --
    -- The mock is named after the operation rather than the field, so that
    -- record gives @ReadFile@ and @ReadFile_@, not @ReadFileImpl@.  Set this
    -- to @""@ if your fields aren't named that way, or if an operation of
    -- yours genuinely ends in @Impl@.
    capStripSuffix :: String,
    -- | Whether to warn about operations that can't be mocked.  This is
    -- mostly useful temporarily, to find out why the generated code doesn't
    -- match your expectations.  Defaults to @'False'@.
    capVerbose :: Bool
  }

instance Default MakeMockableCapabilityOptions where
  def =
    MakeMockableCapabilityOptions
      { capDeriveInstance = True,
        capSuffix = "",
        capStripSuffix = "Impl",
        capVerbose = False
      }

-- | The part of the options the shared generators act on.
generatorOptions :: MakeMockableCapabilityOptions -> GeneratorOptions
generatorOptions options =
  GeneratorOptions
    { genSuffix = capSuffix options,
      genStripSuffix = capStripSuffix options
    }

-- | A capability reified for mocking: the shared description the generators
-- need, plus what only the 'MockableCapability' instance needs.
data CapabilityInstance = CapabilityInstance
  { capInstance :: Instance,
    -- | The record's constructor.
    capCon :: Name,
    -- | Each mockable operation.
    capFields :: [Operation],
    -- | Fields that can't be mocked, which rule out deriving the instance.
    capUnmockable :: [Name]
  }

-- | One mockable operation: its shared description, whose 'methodName' is
-- the record field implementing it, and how the 'MockableCapability'
-- instance must treat each argument.
data Operation = Operation
  { opMethod :: Method,
    -- | For each argument, the arity of the callback if it is one.  A callback
    -- argument is in the capability's @Eff@ scope, and has to be unlifted
    -- before the engine sees it.
    opCallbacks :: [Maybe Int]
  }

-- | Defines all instances necessary to mock the given Bluefin capability,
-- using default options.  The type should be a record of effectful
-- operations, applied to zero or more type arguments, whose last type
-- parameter is its effect scope.
--
-- This defines all of the following instances, if necessary:
--
-- * 'Test.HMock.MockableBase' and the associated 'Test.HMock.Action' and
--   'Test.HMock.Matcher' types, with one constructor per operation.
-- * 'Test.HMock.Bluefin.ExactMatch' instances, so an @Action@ can stand in for a @Matcher@.
-- * 'MockableCapability', whose 'Test.HMock.Bluefin.mocked' builds the
--   capability with every operation delegated to the mock.
--
-- A field must have the form @a -> b -> ... -> 'Eff' e r@, and any type
-- variable in @r@ must have a 'Data.Typeable.Typeable' constraint.  An
-- argument may itself be a callback of that form; it reaches HMock as an
-- @a -> ... -> 'IO' r@, which a response can run with 'Bluefin.IO.effIO'.
-- Fields of any other shape can't be mocked, and if the capability has one you'll need to write
-- the 'MockableCapability' instance yourself, with
-- @'makeMockableCapabilityWithOptions' ... def {'capDeriveInstance' = False}@.
makeMockableCapability :: Q Type -> Q [Dec]
makeMockableCapability qtype = makeMockableCapabilityWithOptions qtype def

-- | Defines all instances necessary to mock the given Bluefin capability,
-- using the provided options.
makeMockableCapabilityWithOptions ::
  Q Type -> MakeMockableCapabilityOptions -> Q [Dec]
makeMockableCapabilityWithOptions qtype capOptions = do
  checkMockableExtensions
  let gen = generatorOptions capOptions
  ty <- qtype
  -- Unlike "Test.HMock.Classic.TH", the type is used as written rather than
  -- generalized first.  A class needs its Action type to stay general so that
  -- several specialized MockT instances can share one MockableBase, but a
  -- capability has no such split: @[t|KVStore String Int|]@ generates a
  -- monomorphic mock and @[t|KVStore|]@ generates a polymorphic one, which is
  -- the choice the caller wants to make.
  ci <- getCapabilityInstance capOptions ty
  when (null (capFields ci)) $
    fail $
      "Cannot mock " ++ pprint ty ++ " because none of its fields has the form"
        ++ " a -> ... -> Eff e r, where e is its last type parameter.  Is the"
        ++ " effect scope the last parameter?  Set capVerbose = True to see why"
        ++ " each field was rejected."
  mockable <- defineMockableInstances gen (capInstance ci)
  capability <- deriveMockableCapability capOptions gen ci
  return (mockable ++ capability)

-- | Reifies a capability record into the same t'Instance' description that
-- "Test.HMock.Classic.TH" builds from a type class, so that the two share all of the
-- 'Test.HMock.Action' and 'Test.HMock.Matcher' generation.
getCapabilityInstance ::
  MakeMockableCapabilityOptions ->
  Type ->
  Q CapabilityInstance
getCapabilityInstance capOptions ty = do
  name <- case unappliedName ty of
    Just n -> return n
    Nothing -> fail "Expected a capability type, but got something else."
  reify name >>= \case
    TyConI (DataD _ _ params _ cons _) -> go name params cons
    TyConI (NewtypeD _ _ params _ con _) -> go name params [con]
    _ ->
      fail $
        "Expected " ++ show name ++ " to be a capability record, but it"
          ++ " wasn't."
  where
    go :: Name -> [TyVarBndr flag] -> [Con] -> Q CapabilityInstance
    go _ [] _ =
      fail $ pprint ty ++ " has no type parameters, so it has no effect scope."
    go name params cons = matchVars ty [] (tvName <$> params)
      where
        headTy = foldl' (\t v -> AppT t (VarT (tvName v))) (ConT name) params

        -- Peels off the arguments the splice applied, mirroring the way
        -- Test.HMock.Classic.TH matches a partially applied class.  The parameters
        -- left over are the capability's own, and the last of those is its
        -- effect scope.
        matchVars :: Type -> [Type] -> [Name] -> Q CapabilityInstance
        matchVars _ _ [] = internalError
        matchVars (AppT _ _) _ [_] =
          fail $ pprint ty ++ " is applied to too many arguments."
        matchVars (AppT a b) ts (_ : ps) =
          checkExt FlexibleInstances >> matchVars a (b : ts) ps
        matchVars _ ts ps = do
          (con, fields) <- getFields headTy cons
          let genVars = init ps
              eVar = last ps
              t = foldl' (\t' v -> AppT t' (VarT v)) ty genVars
              tbl = zip (tvName <$> params) ts
          makeCapabilityInstance capOptions t con tbl genVars eVar fields

    -- The record's constructor and its fields, with field types stated in
    -- terms of the declaration's type variables.  A constructor declared in
    -- GADT syntax reifies wrapped in a 'ForallC' that binds its own variables,
    -- so its result type is unified with the declaration head to rename them.
    getFields :: Type -> [Con] -> Q (Name, [(Name, Type)])
    getFields _ [RecC con fields] =
      return (con, [(n, fieldTy) | (n, _, fieldTy) <- fields])
    getFields headTy [ForallC _ [] con@RecGadtC {}] = getFields headTy [con]
    getFields headTy [RecGadtC [con] fields resTy] =
      unifyTypes resTy headTy >>= \case
        Just tbl ->
          return (con, [(n, substTypeVars tbl fieldTy) | (n, _, fieldTy) <- fields])
        Nothing ->
          fail $
            pprint ty ++ "'s constructor builds a " ++ pprint resTy
              ++ " rather than a " ++ pprint headTy
              ++ ", so it isn't a plain record."
    getFields _ [ForallC _ (_ : _) _] =
      fail $
        pprint ty
          ++ "'s constructor has a context, so it isn't a plain record."
    getFields _ [] = fail $ pprint ty ++ " has no constructors."
    getFields _ [RecGadtC (_ : _ : _) _ _] = tooManyConstructors
    getFields _ (_ : _ : _) = tooManyConstructors
    getFields _ _ =
      fail $
        pprint ty
          ++ " is not a record, so its operations have no names.  A Bluefin"
          ++ " capability should be a record of effectful operations."

    tooManyConstructors :: Q a
    tooManyConstructors =
      fail $
        pprint ty ++ " has more than one constructor, so it isn't a capability."

makeCapabilityInstance ::
  MakeMockableCapabilityOptions ->
  Type ->
  Name ->
  [(Name, Type)] ->
  [Name] ->
  Name ->
  [(Name, Type)] ->
  Q CapabilityInstance
makeCapabilityInstance options ty con tbl genVars eVar fields = do
  processed <- mapM (getOperation eVar tbl) fields
  fieldOrMethods <- sequence (zipWith fieldOrMethod fields processed)
  let (unmockable, mockable) = partitionEithers fieldOrMethods
  return
    CapabilityInstance
      { capInstance =
          Instance
            { instType = ty,
              instGeneralParams = genVars,
              instMethods = opMethod <$> mockable
            },
        capCon = con,
        capFields = mockable,
        capUnmockable = unmockable
      }
  where
    fieldOrMethod ::
      (Name, Type) ->
      Either [String] (Method, [Maybe Int]) ->
      Q (Either Name Operation)
    fieldOrMethod (n, _) (Left warnings) = do
      when (capVerbose options) $ mapM_ reportWarning warnings
      return (Left n)
    fieldOrMethod _ (Right (method, callbacks)) =
      return (Right (Operation method callbacks))

-- | Turns one record field into a mockable t'Method', or explains why it
-- isn't one.
getOperation ::
  Name ->
  [(Name, Type)] ->
  (Name, Type) ->
  Q (Either [String] (Method, [Maybe Int]))
getOperation eVar tbl (name, rawTy) = do
  let (tvs, cx, args, retval) = splitType (substTypeVars tbl rawTy)
  return $ do
    result <- case retval of
      AppT (AppT (ConT eff) (VarT e')) r
        | eff == ''Eff && e' == eVar -> return r
      _ ->
        Left
          [ nameBase name
              ++ " can't be mocked: result is not in the capability's Eff"
              ++ " scope."
          ]
    when (eVar `elem` freeTypeVars result) $
      Left
        [ nameBase name
            ++ " can't be mocked: result type mentions the effect scope."
        ]
    -- The engine matches steps by the return type's TypeRep, so a type
    -- variable in the result must come with a Typeable constraint.  Without
    -- this check the generated 'mocked' instance fails to typecheck, with an
    -- error pointing into the splice rather than at the field.
    unless (all (isVarTypeable cx) (filter (`elem` tvs) (freeTypeVars result))) $
      Left
        [ nameBase name
            ++ " can't be mocked: result type is not Typeable.  Add a Typeable"
            ++ " constraint to its type variables."
        ]
    when (any hasNestedPolyType args) $
      Left
        [ nameBase name
            ++ " can't be mocked: rank-n types nested in arguments."
        ]
    (args', callbacks) <- unzip <$> traverse engineArg args
    return
      ( Method
          { methodName = name,
            methodTyVars = tvs,
            methodCxt = cx,
            methodArgs = args',
            methodResult = result
          },
        callbacks
      )
  where
    isVarTypeable :: Cxt -> Name -> Bool
    isVarTypeable cx' v = AppT (ConT ''Typeable) (VarT v) `elem` cx'

    -- An argument as the engine sees it.  A callback in the capability's Eff
    -- scope becomes an IO function; anything else mentioning the scope can't
    -- be mocked.
    engineArg :: Type -> Either [String] (Type, Maybe Int)
    engineArg argTy
      | eVar `notElem` freeTypeVars argTy = Right (argTy, Nothing)
      | ([], [], params, AppT (AppT (ConT eff) (VarT e')) r) <- splitType argTy,
        eff == ''Eff,
        e' == eVar,
        eVar `notElem` concatMap freeTypeVars (r : params) =
          Right
            ( foldr (\p t -> AppT (AppT ArrowT p) t) (AppT (ConT ''IO) r) params,
              Just (length params)
            )
      | otherwise =
          Left
            [ nameBase name
                ++ " can't be mocked: an argument mentions the effect scope"
                ++ " other than as a callback of the form a -> ... -> Eff e r."
            ]

-- | Generates the 'MockableCapability' instance, which builds the capability
-- record with every operation delegated to the mock.
deriveMockableCapability ::
  MakeMockableCapabilityOptions ->
  GeneratorOptions ->
  CapabilityInstance ->
  Q [Dec]
deriveMockableCapability capOptions gen ci
  | not (capDeriveInstance capOptions) = return []
  | otherwise = do
    let inst = capInstance ci
    needed <-
      isNothing <$> resolveInstance ''MockableCapability [instType inst]
    if not needed
      then return []
      else do
        unless (null (capUnmockable ci)) $
          fail $
            "Cannot derive MockableCapability because " ++ pprint (instType inst)
              ++ " has unmockable operations: "
              ++ intercalate ", " (nameBase <$> capUnmockable ci)
        capVar <- newName "cap"
        fieldExps <- mapM (capabilityField gen capVar) (capFields ci)
        typeableCxt <- constrainVars [conT ''Typeable] (instGeneralParams inst)
        (: [])
          <$> instanceD
            (pure typeableCxt)
            [t|MockableCapability $(pure (instType inst))|]
            [ funD
                'mocked
                [ clause
                    [varP capVar]
                    (normalB (recConE (capCon ci) (pure <$> fieldExps)))
                    []
                ]
            ]

-- | Builds one field of the capability record, delegating to HMock.  Callback
-- arguments are unlifted to 'IO' on the way.
capabilityField :: GeneratorOptions -> Name -> Operation -> Q (Name, Exp)
capabilityField options capVar op = do
  let method = opMethod op
  argVars <- replicateM (length (methodArgs method)) (newName "a")
  unliftVar <- newName "unlift"
  defaultCxt <- simplifyContext [AppT (ConT ''Default) (methodResult method)]
  let delegate = case defaultCxt of
        Just [] -> varE 'mockMethod
        _ -> varE 'mockDefaultlessMethod
      argExp v Nothing = varE v
      argExp v (Just 0) = [|$(varE unliftVar) $(varE v)|]
      argExp v (Just n) = do
        xs <- replicateM n (newName "x")
        lamE
          (varP <$> xs)
          [|$(varE unliftVar) $(foldl' appE (varE v) (varE <$> xs))|]
      action =
        foldl'
          (\e (v, cb) -> appE e (argExp v cb))
          (conE (getActionName options method))
          (zip argVars (opCallbacks op))
      -- Frozen so that the backtrace stops at the 'mocked' call site rather
      -- than reporting the splice as the innermost frame.
      call = [|withFrozenCallStack ($delegate $(varE capVar) $action)|]
  body <-
    if any isJust (opCallbacks op)
      then [|withUnlift $(varE capVar) $(lamE [varP unliftVar] call)|]
      else call
  return
    ( methodName method,
      if null argVars then body else LamE (VarP <$> argVars) body
    )
