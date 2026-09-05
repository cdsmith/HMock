{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

-- | Template Haskell for deriving the parts of a mock that don't depend on
-- what is being mocked: the 'Action' and 'Matcher' types, their
-- 'MockableBase' instance, and the 'ExactMatch' instances that let an
-- @Action@ stand in for a @Matcher@.
--
-- A Template Haskell front end reifies whatever kind of interface it
-- understands into an t'Instance', then hands that to
-- 'defineMockableInstances'.  Nothing here depends on what that interface was.
module Test.HMock.Internal.MakeMockable
  ( -- * Describing what is being mocked
    Instance (..),
    Method (..),
    GeneratorOptions (..),

    -- * Generating the shared instances
    checkMockableExtensions,
    defineMockableInstances,

    -- * Naming the generated constructors
    methodLabel,
    getActionName,
    getMatcherName,

    -- * Utilities
    checkExt,
    internalError,
  )
where

import Control.Monad (replicateM, unless, when)
import Control.Monad.Extra (concatMapM)
import Data.Bool (bool)
import Data.Char (toUpper)
import qualified Data.Kind
import Data.List (intercalate, stripPrefix)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isNothing)
import Data.Proxy (Proxy)
import Data.Typeable (Typeable, typeRep)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (ErrorMessage (Text, (:$$:), (:<>:)), Symbol, TypeError)
import Language.Haskell.TH hiding (Match, match)
import Language.Haskell.TH.Syntax (Lift (lift))
import Test.HMock.Internal.TH
import Test.HMock.Mockable (ExactMatch (..), MatchResult (..), MockableBase (..))
import Test.Predicates (Predicate (..), eq)

-- | The choices the generators here need made for them.  Each front end
-- exposes its own options record to users and projects it down to this.
data GeneratorOptions = GeneratorOptions
  { -- | Suffix to add to 'Action' and 'Matcher' constructor names.
    genSuffix :: String,
    -- | Suffix to strip from a method's declared name to get the name it is
    -- mocked under.  See 'methodLabel'.
    genStripSuffix :: String
  }

-- | A description of the interface being mocked, in the form the generators
-- here need it.
data Instance = Instance
  { instType :: Type,
    instGeneralParams :: [Name],
    instMethods :: [Method]
  }
  deriving (Show)

-- | One mockable method of an interface.
data Method = Method
  { -- | The method's declared name.  The name it is mocked under, which
    -- names its 'Action' and 'Matcher' constructors, its
    -- 'GHC.TypeLits.Symbol', and how it prints in errors, is 'methodLabel'.
    methodName :: Name,
    methodTyVars :: [Name],
    methodCxt :: Cxt,
    methodArgs :: [Type],
    methodResult :: Type
  }
  deriving (Show)

-- | The name a method is mocked under: its declared name with any configured
-- suffix stripped, unless that would leave nothing.  Everything the user sees
-- of the method is derived from this.
methodLabel :: GeneratorOptions -> Method -> String
methodLabel options method
  | null suffix = name
  | Just base <- stripPrefix (reverse suffix) (reverse name),
    not (null base) =
    reverse base
  | otherwise = name
  where
    suffix = genStripSuffix options
    name = nameBase (methodName method)

-- | Fails if two methods would be mocked under the same name, which would
-- give the 'Action' and 'Matcher' types duplicate constructors.
checkDistinctNames :: GeneratorOptions -> Instance -> Q ()
checkDistinctNames options inst = case clashes of
  [] -> return ()
  _ ->
    fail $
      "Cannot mock " ++ pprint (instType inst) ++ " because "
        ++ intercalate ", and " (map describe clashes)
        ++ "."
  where
    byAction =
      Map.fromListWith
        (flip (++))
        [ (nameBase (getActionName options m), [nameBase (methodName m)])
          | m <- instMethods inst
        ]
    clashes = [(action, names) | (action, names@(_ : _ : _)) <- Map.toList byAction]
    describe (action, [a, b]) = a ++ " and " ++ b ++ " would both be mocked as " ++ action
    describe (action, names) =
      intercalate ", " names ++ " would all be mocked as " ++ action

isKnownType :: Method -> Type -> Bool
isKnownType method ty = null tyVars && null cx
  where
    (tyVars, cx) =
      relevantContext ty (methodTyVars method, methodCxt method)

withMethodParams :: GeneratorOptions -> Instance -> Method -> TypeQ -> TypeQ
withMethodParams options inst method t =
  [t|
    $t
      $(pure (instType inst))
      $(litT (strTyLit (methodLabel options method)))
      $(pure (methodResult method))
    |]

defineActionType :: GeneratorOptions -> Instance -> DecQ
defineActionType options inst = do
  kind <- [t|Symbol -> Data.Kind.Type -> Data.Kind.Type|]
  let cons = actionConstructor options inst <$> instMethods inst
  dataInstD
    (pure [])
    ''Action
    [pure (instType inst)]
    (Just kind)
    cons
    []

actionConstructor :: GeneratorOptions -> Instance -> Method -> ConQ
actionConstructor options inst method = do
  forallC [] (return (methodCxt method)) $
    gadtC
      [getActionName options method]
      [ return (Bang NoSourceUnpackedness NoSourceStrictness, argTy)
        | argTy <- methodArgs method
      ]
      (withMethodParams options inst method [t|Action|])

-- | The name of the 'Action' constructor for a method: its label,
-- capitalized, plus any configured suffix.
getActionName :: GeneratorOptions -> Method -> Name
getActionName options method =
  mkName (map toUpper (take 1 name) ++ drop 1 name ++ genSuffix options)
  where
    name = methodLabel options method

defineMatcherType :: GeneratorOptions -> Instance -> Q Dec
defineMatcherType options inst = do
  kind <- [t|Symbol -> Data.Kind.Type -> Data.Kind.Type|]
  let cons = matcherConstructor options inst <$> instMethods inst
  dataInstD
    (pure [])
    ''Matcher
    [pure (instType inst)]
    (Just kind)
    cons
    []

matcherConstructor :: GeneratorOptions -> Instance -> Method -> ConQ
matcherConstructor options inst method = do
  gadtC
    [getMatcherName options method]
    [ (Bang NoSourceUnpackedness NoSourceStrictness,) <$> mkPredicate argTy
      | argTy <- methodArgs method
    ]
    (withMethodParams options inst method [t|Matcher|])
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

-- | The name of the 'Matcher' constructor for a method, which is the
-- 'Action' constructor's name with a trailing underscore.
getMatcherName :: GeneratorOptions -> Method -> Name
getMatcherName options method =
  mkName (nameBase (getActionName options method) ++ "_")

defineShowAction :: GeneratorOptions -> [Method] -> Q Dec
defineShowAction options methods =
  funD 'showAction (showActionClause options <$> methods)

showActionClause :: GeneratorOptions -> Method -> Q Clause
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
            ( $(lift (methodLabel options method)) :
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

defineShowMatcher :: GeneratorOptions -> [Method] -> Q Dec
defineShowMatcher options methods = do
  clauses <- concatMapM (showMatcherClauses options) methods
  funD 'showMatcher clauses

showMatcherClauses :: GeneratorOptions -> Method -> Q [ClauseQ]
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
        (body (methodLabel options method) printedArgs)
        [],
      clause
        [ conP 'Nothing [],
          conP (getMatcherName options method) polyMatcherArgs
        ]
        (body (methodLabel options method) printedPolyArgs)
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

defineMatchAction :: GeneratorOptions -> [Method] -> Q Dec
defineMatchAction options methods =
  funD 'matchAction (matchActionClause options <$> methods)

matchActionClause :: GeneratorOptions -> Method -> Q Clause
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

defineExactMatchInstances :: GeneratorOptions -> Instance -> Q [Dec]
defineExactMatchInstances options inst =
  mapM (defineExactMatchInstance options inst) (instMethods inst)

type ComplexActionMessage name =
  ( 'Text "Method " ':<>: 'Text name
      ':<>: 'Text " is too complex to expect with an Action."
  )
    ':$$: 'Text "Suggested fix: Use a Matcher instead of an Action."

defineExactMatchInstance :: GeneratorOptions -> Instance -> Method -> Q Dec
defineExactMatchInstance options inst method = do
  maybeCxt <- wholeCxt (methodArgs method)
  argVars <- replicateM (length (methodArgs method)) (newName "a")
  case maybeCxt of
    Just cx -> do
      instanceD
        (pure (methodCxt method ++ cx))
        (withMethodParams options inst method [t|ExactMatch|])
        [ funD
            'exactMatcher
            [ clause
                [conP (getActionName options method) (map varP argVars)]
                (normalB (makeBody argVars (conE (getMatcherName options method))))
                []
            ]
        ]
    _ -> do
      checkExt UndecidableInstances
      instanceD
        ( (: [])
            <$> [t|
              TypeError
                ( ComplexActionMessage
                    $(litT $ strTyLit $ methodLabel options method)
                )
              |]
        )
        (withMethodParams options inst method [t|ExactMatch|])
        [ funD
            'exactMatcher
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

-- | Fails the splice unless the given language extension is enabled.
checkExt :: Extension -> Q ()
checkExt e = do
  enabled <- isExtEnabled e
  unless enabled $
    fail $ "Please enable " ++ show e ++ " to generate this mock."

-- | Reports a bug in HMock itself, from a case that should be unreachable.
internalError :: HasCallStack => Q a
internalError = error "Internal error in HMock.  Please report this as a bug."

-- | Checks for the language extensions that generated mocks need.
--
-- Call this before reifying anything, so that a missing extension is reported
-- in preference to any other complaint about the type being mocked.
checkMockableExtensions :: Q ()
checkMockableExtensions = do
  checkExt DataKinds
  checkExt FlexibleInstances
  checkExt GADTs
  checkExt MultiParamTypeClasses
  checkExt ScopedTypeVariables
  checkExt TypeFamilies

-- | Generates the 'MockableBase' instance (with its 'Action' and 'Matcher'
-- types) and the 'ExactMatch' instances for the 'Action' constructors.
--
-- Instances that already exist are left alone, so a caller may generate the
-- pieces for a more general type first and then specialize.
defineMockableInstances :: GeneratorOptions -> Instance -> Q [Dec]
defineMockableInstances options inst = do
  when (null (instMethods inst)) $
    fail $
      "Cannot mock " ++ pprint (instType inst)
        ++ " because it has no mockable methods."
  checkDistinctNames options inst

  typeableCxt <- constrainVars [conT ''Typeable] (instGeneralParams inst)

  needsMockableBase <-
    isNothing <$> resolveInstance ''MockableBase [instType inst]
  mockableBase <-
    if needsMockableBase
      then do
        base <-
          instanceD
            (pure typeableCxt)
            [t|MockableBase $(pure (instType inst))|]
            [ defineActionType options inst,
              defineMatcherType options inst,
              defineShowAction options (instMethods inst),
              defineShowMatcher options (instMethods inst),
              defineMatchAction options (instMethods inst)
            ]
        exact <- defineExactMatchInstances options inst
        return (base : exact)
      else return []

  return mockableBase
