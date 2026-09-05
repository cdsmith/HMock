{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Mocking for [Bluefin](https://hackage.haskell.org/package/bluefin)
-- capabilities: everything in "Test.HMock.Bluefin" apart from the Template
-- Haskell, which lives in "Test.HMock.Bluefin.TH" and builds on this module.
-- You normally import "Test.HMock.Bluefin", which re-exports both.
module Test.HMock.Bluefin.Mock
  ( -- * Running mocks
    MockSource,
    runMock,

    -- * Mock capabilities
    Mock,
    MockableCapability (..),
    newMock,
    newNamedMock,
    mockName,

    -- * Nested expectations
    nestMockSource,
    adoptMock,

    -- * Expectations
    Target (Accepts),
    ExpectContext (..),
    Expectation,
    ExpectMode (..),
    Expects,

    -- * Defaults and side effects
    byDefault,
    allowUnexpected,
    whenever,

    -- * Checking and configuring a test
    Severity (..),
    setAmbiguityCheck,
    setUninterestingActionCheck,
    setUnexpectedActionCheck,
    setUnmetExpectationCheck,
    describeExpectations,
    verifyExpectations,

    -- * Delegating operations to HMock
    mockMethod,
    mockDefaultlessMethod,
    withUnlift,

    -- * Rules and responses
    Rule,
    Expectable (..),
    (|->),
    (|=>),
    WholeMethodMatcher (SuchThat),

    -- * Re-exports
    module Test.HMock.Mockable,
    module Test.HMock.Multiplicity,
  )
where

import Bluefin.Compound (mapHandle)
import Bluefin.Eff (Eff, Effects, type (:&), type (<:))
import Bluefin.IO (IOE, effIO, withEffToIO_)
import Test.HMock.Bluefin.Internal
import Data.Default (Default)
import Data.Kind (Constraint, Type)
import Data.Maybe (listToMaybe)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Test.HMock.Internal.ExpectSet
  ( ExpectSet (..),
    excess,
    formatExpectSet,
  )
import qualified Test.HMock.Internal.Rule as Core
import Test.HMock.Internal.Rule (WholeMethodMatcher (..))
import Test.HMock.Internal.State
  ( MockState (..),
    Severity (..),
    askState,
    describeExpectationsWith,
    expectThisSet,
    initMockState,
    liftSTM,
    newInstanceId,
    reportFault,
    runMockSTM,
  )
import Test.HMock.Internal.Step
  ( SingleRule ((:->)),
    Step (Step),
    expandRepeatRule,
    expandRule,
  )
import Test.HMock.Internal.Util (InstanceId (instanceName), Located (Loc))
import Test.HMock.Internal.Match (mockDefaultlessMethodWith, mockMethodWith)
import Test.HMock.Mockable
import Test.HMock.Multiplicity
import UnliftIO (TVar, atomically, modifyTVar', readTVar, writeTVar)

-- | Runs a test with mocks available, and verifies at the end that all
-- expectations were met.
--
-- An 'IOE' capability is required because HMock's expectation state is
-- mutable and mocked operations can perform I/O, so a mock may not be run
-- under 'Bluefin.Eff.runPureEff'.
runMock ::
  forall es e1 a.
  (HasCallStack, e1 <: es) =>
  IOE e1 ->
  (forall e. MockSource e (e :& es) e -> Eff (e :& es) a) ->
  Eff es a
runMock io test = do
  st <- effIO io (initMockState Nothing)
  a <- withScope io $ \io' unlift ->
    test (MockSource st io' unlift :: MockSource es (es :& es) es)
  verifyState io st
  return a

-- | A Bluefin capability whose operations can be delegated to HMock.
--
-- You will normally derive this with
-- 'Test.HMock.Bluefin.TH.makeMockableCapability', since it is boilerplate.
type MockableCapability :: (Effects -> Type) -> Constraint

class (MockableBase h) => MockableCapability h where
  -- | Builds the plain capability record, with every operation delegated to
  -- the mock, ready to hand to the code under test.
  --
  -- The record shares the handle's own effect scope.  t'Mock' is a
  -- 'Bluefin.Compound.Handle', so use 'Bluefin.Compound.mapHandle' first if you need it at a
  -- wider one.
  --
  -- The call site is recorded, so that an error raised by one of the record's
  -- operations points back to where the record was built.
  mocked :: (HasCallStack) => Mock root resp h e -> h e

-- | Creates a new mocked instance of a capability, in the same way
-- 'Bluefin.StateSource.newState' creates a state.  The handle carries its
-- source's effect tag, so use 'Bluefin.Compound.mapHandle' to use it at a
-- wider scope.
--
-- Each call creates a distinct instance, so a test needing two of the same
-- capability can tell them apart:
--
-- @
-- src <- 'newMock' source
-- dst <- newMock source
-- 'expect' src '$' ReadFile "a.txt" '|->' "contents"
-- 'expect' dst '$' WriteFile "b.txt" "contents"
-- copyBetween ('mocked' src) (mocked dst)
-- @
--
-- Reading through @dst@ in that test is an unexpected action, not a match on
-- @src@'s expectation.
--
-- If every mock of a capability should start with the same defaults, wrap
-- this in a function that adds them:
--
-- @
-- mockFileSystem :: (e '<:' es) => MockSource root resp e -> Eff es (Mock root resp FileSystem e)
-- mockFileSystem source = do
--   fs <- newMock source
--   'allowUnexpected' fs '$' WriteFile_ 'Test.Predicates.anything' anything
--   return fs
-- @
newMock ::
  forall h root resp es e1.
  (MockableCapability h, e1 <: es) =>
  MockSource root resp e1 ->
  Eff es (Mock root resp h e1)
newMock = makeMock Nothing

-- | Like 'newMock', but gives the instance a name, which error messages and
-- 'describeExpectations' use in place of its number:
--
-- > Wrong instance: readFile "a.txt" on instance dst
--
-- Names are unique within a test.  If the name is already in use, a numeric
-- suffix is added: @"fs"@, then @"fs_2"@, then @"fs_3"@.
newNamedMock ::
  forall h root resp es e1.
  (MockableCapability h, e1 <: es) =>
  String ->
  MockSource root resp e1 ->
  Eff es (Mock root resp h e1)
newNamedMock name = makeMock (Just name)

makeMock ::
  forall h root resp es e1.
  (MockableCapability h, e1 <: es) =>
  Maybe String ->
  MockSource root resp e1 ->
  Eff es (Mock root resp h e1)
makeMock name (MockSource st io unlift) = effIO io $
  atomically $ runMockSTM st $ do
    (\cid -> Mock st cid io unlift) <$> newInstanceId name

-- | The name of a mocked instance, as it appears in messages: the name given
-- to 'newNamedMock', or @#@ and its number for a mock from 'newMock'.
mockName :: Mock root resp h e -> String
mockName (Mock _ cid _ _) = instanceName cid

-- | Starts a nested block with its own layer of expectations, which must be
-- met before the block ends.
--
-- Mocks are not nested automatically: the layer a mock talks to is fixed when
-- 'mocked' builds its capability record, so a mock has to be adopted into the
-- nested source with 'adoptMock' before it will see the new layer.  Shadowing
-- the outer binding is the safest way to write that:
--
-- @
-- 'nestMockSource' source '$' \\nested -> do
--   fs <- 'adoptMock' nested fs
--   clock <- adoptMock nested clock
--   'expect' fs '$' ...
--   codeUnderTest ('mocked' fs) (mocked clock)
-- @
--
-- One nested source serves any number of adopted mocks, so a plan may still
-- be sequenced across all of them.  Expectations from the enclosing layers
-- remain visible to calls made through an adopted mock.  The enclosing
-- layers are those of the source passed in, so to nest again inside a nested
-- block, pass its source rather than the original one.
--
-- Beware: needing this may signify that a test is doing too much.  Consider
-- splitting it instead.
nestMockSource ::
  forall es e root resp a.
  (HasCallStack, e <: es) =>
  MockSource root resp e ->
  (forall e'. MockSource root (e' :& es) e' -> Eff (e' :& es) a) ->
  Eff es a
nestMockSource (MockSource st io _) test = do
  st' <- effIO io (initMockState (Just st))
  a <- withScope io $ \io' unlift ->
    test (MockSource st' io' unlift :: MockSource root (es :& es) es)
  verifyState io st'
  return a

-- | Adopts an existing mock into another source's layer, keeping the same
-- capability instance.  Expectations set on the result, and calls made
-- through @'mocked'@ of it, belong to that layer.
--
-- The original mock is unaffected and still refers to its own layer, so use
-- the adopted one consistently for both.
--
-- Adoption works in either direction.  A mock created inside a nested block
-- may be adopted into an enclosing source, and expectations set on the
-- result then belong to that enclosing layer, just as if they had been set
-- on a mock made there.  Adoption stays within one 'runMock', since @root@
-- must match.
adoptMock ::
  (e <: es, e' <: es) =>
  MockSource root resp' e' ->
  Mock root resp h e ->
  Eff es (Mock root resp' h e')
adoptMock (MockSource st io unlift) (Mock _ cid _ _) = return (Mock st cid io unlift)

-- | Implements a capability operation by delegating to the mock framework.
-- If the operation is called unexpectedly, an exception is thrown.  An
-- expected call without a specified response returns the default value.
mockMethod ::
  ( HasCallStack,
    c <: es,
    MockableMethod h name r,
    Default r
  ) =>
  Mock root resp h c ->
  Action h name r ->
  Eff es r
mockMethod cap action =
  withFrozenCallStack $
    effIO (mockIO cap) (mockMethodWith (mockState cap) (mockId cap) action)

-- | Like 'mockMethod', but for return types with no 'Default' instance.  An
-- expected call without a specified response returns 'undefined'.
mockDefaultlessMethod ::
  ( HasCallStack,
    c <: es,
    MockableMethod h name r
  ) =>
  Mock root resp h c ->
  Action h name r ->
  Eff es r
mockDefaultlessMethod cap action =
  withFrozenCallStack $
    effIO
      (mockIO cap)
      (mockDefaultlessMethodWith (mockState cap) (mockId cap) action)

-- | Runs part of a mocked operation with an unlifter for the mock's own
-- scope.
--
-- A callback argument of type @a -> ... -> 'Eff' e r@ reaches HMock as an
-- @a -> ... -> 'IO' r@, since the engine stores actions in 'IO'.  Generated
-- code uses this to convert such arguments; you only need it yourself when
-- writing a 'MockableCapability' instance by hand for an operation that has
-- them:
--
-- @
-- 'mocked' cap = MkResource
--   { withResourceImpl = \\name k ->
--       'withUnlift' cap (\\unlift -> 'mockMethod' cap (WithResource name (unlift . k)))
--   }
-- @
--
-- A response receives the 'IO' function, and can run it with
-- 'Bluefin.IO.effIO'.
withUnlift ::
  Mock root resp h e ->
  ((forall r. Eff e r -> IO r) -> Eff e a) ->
  Eff e a
withUnlift cap k = withEffToIO_ (mockIO cap) (\unlift -> unlift (k unlift))

-- | Matches an operation, and optionally responds to it.
--
-- Match with an 'Action' for exact arguments, or a 'Matcher' for predicates.
-- Responses attach with '|->' and '|=>': none means the default, several are
-- used in turn, repeating the last.  A response runs in @'Eff' es@, the scope
-- at the expectation, so it can reach anything there -- the mock included.
--
-- @
-- 'expect' fs '$'
--   ReadFile_ 'Test.Predicates.anything'
--     '|->' "lorem ipsum"
--     '|=>' \(ReadFile path) -> do
--       'Bluefin.IO.effIO' io (putStrLn ("read " ++ path))
--       return "the second time"
-- @
data Rule cls name es r where
  (:=>) ::
    WholeMethodMatcher cls name r ->
    [Action cls name r -> Eff es r] ->
    Rule cls name es r

-- | Class for things that can be expected.  This includes 'Rule's, but also
-- bare 'Matcher's and 'Action's with no explicit response.
class Expectable cls name es r ex | ex -> cls name r where
  -- | Converts an expectable to a 'Rule' meaning the same thing.
  toRule :: ex -> Rule cls name es r

-- This matches any scope and then equates it with the one the expectation is
-- being written in, so that a rule built by '|->' or '|=>' picks up its scope
-- from the surrounding 'expect'.
instance (es ~ es') => Expectable cls name es r (Rule cls name es' r) where
  toRule = id

instance Expectable cls name es r (WholeMethodMatcher cls name r) where
  toRule m = m :=> []

instance Expectable cls name es r (Matcher cls name r) where
  toRule m = JustMatcher m :=> []

-- | An 'Action' stands in for the 'Matcher' that matches it exactly.
instance
  ExactMatch cls name r =>
  Expectable cls name es r (Action cls name r)
  where
  toRule a = JustMatcher (exactMatcher a) :=> []

-- | Attaches a response to an expectation.  This is a flexible response,
-- which can look at the arguments, perform effects, set up more expectations,
-- and so on.  The matching 'Action' is passed to it.
(|=>) ::
  Expectable cls name es r ex =>
  ex ->
  (Action cls name r -> Eff es r) ->
  Rule cls name es r
e |=> r = m :=> (rs ++ [r]) where m :=> rs = toRule e

infixl 1 |=>

-- | Attaches a return value to an expectation.  More convenient than '|=>'
-- when you just want to return a known result.  @e '|->' r@ means the same as
-- @e '|=>' 'const' ('return' r)@.
(|->) :: Expectable cls name es r ex => ex -> r -> Rule cls name es r
m |-> r = m |=> const (return r)

infixl 1 |->

-- | Converts a rule for the engine, unlifting each response to 'IO'.
toMockRule :: Unlifter resp -> Rule cls name resp r -> Core.Rule cls name IO r
toMockRule (Unlifter unlift) (m :=> rs) =
  m Core.:=> map (\f action -> unlift (f action)) rs

-- | A plan for what a test expects, built but not yet installed.
--
-- You only name this type when you factor part of a plan out into its own
-- definition.  Otherwise it's just the context in which the arguments to
-- 'inSequence' and friends are built.
type role Expectation nominal nominal nominal

-- Along with the steps, a plan carries the state and IOE that 'install' needs,
-- taken from any of its mocks; there are none only if the plan has no mocks.
data Expectation (resp :: Effects) (es :: Effects) a
  = Expectation (Maybe (MockState IO, IOE es)) (ExpectSet (Step IO))

-- | Combines built plans.  The @resp@ index guarantees every child belongs to
-- the same layer, so any of their states will do: 'runMock' and
-- 'nestMockSource' each put a fresh skolem into @resp@, and 'adoptMock'
-- re-tags a mock with its target's @resp@, so one @resp@ names one layer.
combine ::
  ([ExpectSet (Step IO)] -> ExpectSet (Step IO)) ->
  [Expectation resp es a] ->
  Expectation resp es b
combine fold es =
  Expectation
    (listToMaybe [h | Expectation (Just h) _ <- es])
    (fold [steps | Expectation _ steps <- es])

-- | The error for @'anyOf' []@.
emptyAnyOf :: String
emptyAnyOf = "anyOf requires at least one alternative; anyOf [] can never be satisfied."

reshape ::
  (ExpectSet (Step IO) -> ExpectSet (Step IO)) ->
  Expectation resp es a ->
  Expectation resp es b
reshape f (Expectation h steps) = Expectation h (f steps)

-- | Ties a rule's effect scope to the plan's.  'Expectable' can't do this
-- through its fundep, because a bare 'Action' determines the capability and
-- return type but says nothing about the scope its responses run in.
buildExpectation ::
  (c <: es, Target t resp, Accepts t h) =>
  t c ->
  (Core.Rule h name IO r -> ExpectSet (Step IO)) ->
  Rule h name resp r ->
  Expectation resp es ()
buildExpectation tgt expand rule =
  Expectation
    (Just (targetState tgt, mapHandle (targetIO tgt)))
    (expand (toMockRule (targetUnlift tgt) rule))

install :: Expectation resp es a -> Eff es ()
install (Expectation Nothing _) = return ()
install (Expectation (Just (st, io)) steps) =
  effIO io $ atomically $ runMockSTM st (expectThisSet steps)

-- | Whether a combinator installs its expectation where it is written, or
-- builds it as a value to nest inside a compound one.  See 'Expects'.
data ExpectMode
  = -- | Install it immediately, in @'Eff' es@.
    Installing
  | -- | Build an t'Expectation', for nesting.
    Building

-- | What a combinator yields in a given 'ExpectMode': an @'Eff' es@ action
-- that installs the expectation, or an t'Expectation' to nest in a compound
-- one.  The mode is determined by the result type, which is why the same
-- combinator names serve for both:
--
-- @
-- 'expect' fs '$' ReadFile "a.txt" '|->' "contents"     -- installed: Eff es ()
-- 'inSequence'                                        -- installed: Eff es ()
--   [ 'expect' clock '$' CurrentTime '|->' 1234,        -- built: Expectation resp es ()
--     'expect' fs '$' WriteFile "log.1234" "entry"
--   ]
-- @
type family Expects (t :: ExpectMode) (resp :: Effects) (es :: Effects) =
              (r :: Type -> Type)
            | r -> t es
  where
  Expects 'Installing resp es = Eff es
  Expects 'Building resp es = Expectation resp es

-- | The combinators for building a test's execution plan.
--
-- Every combinator names its 'Target': a t'Mock', for one instance, or a
-- t'MockSource', for every instance of the capability the rule mentions.  A
-- compound one requires all of its children to belong to the same layer,
-- which the type records, so a plan cannot mix capabilities from different
-- t'MockSource's.
--
-- The same names work in both contexts: at @'Eff' es@ an expectation is
-- installed as soon as it is written, and at t'Expectation' it is built up for
-- nesting.  See 'Expects'.
type ExpectContext :: ExpectMode -> Constraint

class ExpectContext t where
  -- | Expects an operation to be performed once per given response, or
  -- exactly once if there is no response.
  --
  -- @
  --   fs <- 'newMock' source
  --   'expect' fs '$'
  --     ReadFile "foo.txt"
  --       '|->' "lorem ipsum"
  --       '|->' "oops, the file changed out from under me!"
  -- @
  expect ::
    ( HasCallStack,
      c <: es,
      MockableMethod h name r,
      Expectable h name resp r expectable,
      Target tgt resp,
      Accepts tgt h
    ) =>
    tgt c ->
    expectable ->
    Expects t resp es ()

  -- | Expects an operation to be performed some number of times.
  expectN ::
    ( HasCallStack,
      c <: es,
      MockableMethod h name r,
      Expectable h name resp r expectable,
      Target tgt resp,
      Accepts tgt h
    ) =>
    Multiplicity ->
    tgt c ->
    expectable ->
    Expects t resp es ()

  -- | Specifies a response if a matching operation happens, without expecting
  -- it.  Equivalent to @'expectN' 'anyMultiplicity'@.
  expectAny ::
    ( HasCallStack,
      c <: es,
      MockableMethod h name r,
      Expectable h name resp r expectable,
      Target tgt resp,
      Accepts tgt h
    ) =>
    tgt c ->
    expectable ->
    Expects t resp es ()

  -- | Creates a sequential expectation.  Other operations can still happen
  -- during the sequence, but these must happen in this order.  A sequence may
  -- span several capabilities, so long as they share a layer.  An empty list
  -- expects nothing.
  --
  -- @
  --   'inSequence'
  --     [ 'expect' clock '$' CurrentTime '|->' 1234,
  --       'expect' fs '$' WriteFile "log.1234" "entry"
  --     ]
  -- @
  inSequence ::
    [Expectation resp es ()] ->
    Expects t resp es ()

  -- | Combines expectations that may occur in any order.  An empty list
  -- expects nothing.
  inAnyOrder ::
    [Expectation resp es ()] ->
    Expects t resp es ()

  -- | Combines expectations, requiring exactly one of them to occur.  The
  -- list must not be empty, since a choice among no alternatives can never be
  -- satisfied.  @'anyOf' []@ is an error.
  anyOf ::
    HasCallStack =>
    [Expectation resp es ()] ->
    Expects t resp es ()

  -- | Expects a compound expectation to happen a number of times.  Different
  -- occurrences may interleave.
  times ::
    Multiplicity ->
    Expectation resp es () ->
    Expects t resp es ()

  -- | Expects a compound expectation to happen a number of times, each
  -- occurrence finishing before the next begins.
  consecutiveTimes ::
    Multiplicity ->
    Expectation resp es () ->
    Expects t resp es ()

instance ExpectContext 'Building where
  expect tgt e =
    buildExpectation tgt (expandRule (targetId tgt) callStack) (toRule e)
  expectN mult tgt e =
    buildExpectation
      tgt
      (expandRepeatRule (targetId tgt) mult callStack)
      (toRule e)
  expectAny tgt e =
    buildExpectation
      tgt
      (expandRepeatRule (targetId tgt) anyMultiplicity callStack)
      (toRule e)
  inSequence = combine (foldr ExpectSequence ExpectNothing)
  inAnyOrder = combine (foldr ExpectInterleave ExpectNothing)
  anyOf [] = error emptyAnyOf
  anyOf es = combine (foldr1 ExpectEither) es
  times mult = reshape (ExpectMulti mult)
  consecutiveTimes mult = reshape (ExpectConsecutive mult)

instance ExpectContext 'Installing where
  expect cap e = withFrozenCallStack $ install (expect @'Building cap e)
  expectN mult cap e =
    withFrozenCallStack $ install (expectN @'Building mult cap e)
  expectAny cap e = withFrozenCallStack $ install (expectAny @'Building cap e)
  inSequence es = install (inSequence @'Building es)
  inAnyOrder es = install (inAnyOrder @'Building es)
  anyOf es = withFrozenCallStack $ install (anyOf @'Building es)
  times mult e = install (times @'Building mult e)
  consecutiveTimes mult e = install (consecutiveTimes @'Building mult e)

-- | Sets a default response for /expected/ matching calls through the
-- target.  The new default only applies to calls for which an
-- expectation exists but lacks an explicit response.
byDefault ::
  forall h name c resp es r t.
  (c <: es, MockableMethod h name r, Target t resp, Accepts t h) =>
  t c ->
  Rule h name resp r ->
  Eff es ()
byDefault tgt rule = case toMockRule (targetUnlift tgt) rule of
  m Core.:=> [resp] -> addStep tgt mockDefaults (m :-> Just resp)
  _ -> error "Defaults must have exactly one response."

-- | Adds a handler for unexpected calls through the target.
-- Matching calls will not fail, but will use a default response instead.
-- If the rule has a response, it is used for unexpected calls in preference
-- to any 'byDefault' rule; otherwise the 'byDefault' rule applies.  Expected
-- calls never use the response given here.
allowUnexpected ::
  forall h name c resp es r rule t.
  ( c <: es,
    MockableMethod h name r,
    Expectable h name resp r rule,
    Target t resp,
    Accepts t h
  ) =>
  t c ->
  rule ->
  Eff es ()
allowUnexpected tgt e =
  case toMockRule (targetUnlift tgt) (toRule e :: Rule h name resp r) of
    _ Core.:=> (_ : _ : _) ->
      error "allowUnexpected may not have multiple responses."
    m Core.:=> resps ->
      addStep tgt mockAllowUnexpected (m :-> listToMaybe resps)

-- | Adds a side effect, which happens whenever a matching call occurs through
-- the target, in addition to the usual response.
whenever ::
  forall h name c resp es r t.
  (c <: es, MockableMethod h name r, Target t resp, Accepts t h) =>
  t c ->
  Rule h name resp r ->
  Eff es ()
whenever tgt rule = case toMockRule (targetUnlift tgt) rule of
  m Core.:=> [resp] -> addStep tgt mockSideEffects (m :-> Just resp)
  _ -> error "Side effects must have exactly one response."

-- | Adds a standing rule to one of the target's layer's lists.  Standing
-- rules never appear in messages, so they carry no source location.
addStep ::
  forall t resp c es h name r.
  (c <: es, Target t resp, MockableMethod h name r) =>
  t c ->
  (MockState IO -> TVar [Step IO]) ->
  SingleRule h name IO r ->
  Eff es ()
addStep tgt field rule = effIO (targetIO tgt) $
  atomically $ runMockSTM (targetState tgt) $ do
    state <- askState
    liftSTM $ modifyTVar' (field state) (Step (targetId tgt) (Loc Nothing rule) :)

-- | Sets the severity for ambiguous actions, which match expectations in more
-- than one way.  Defaults to 'Ignore'.
setAmbiguityCheck :: (e1 <: es) => MockSource root resp e1 -> Severity -> Eff es ()
setAmbiguityCheck = setSeverity mockAmbiguitySeverity

-- | Sets the severity for uninteresting actions, for which no expectation or
-- other configuration mentions the operation at all.  Defaults to 'Error'.
setUninterestingActionCheck :: (e1 <: es) => MockSource root resp e1 -> Severity -> Eff es ()
setUninterestingActionCheck = setSeverity mockUninterestingSeverity

-- | Sets the severity for unexpected actions, which match no expectation and
-- aren't allowed by 'allowUnexpected'.  Defaults to 'Error'.
setUnexpectedActionCheck :: (e1 <: es) => MockSource root resp e1 -> Severity -> Eff es ()
setUnexpectedActionCheck = setSeverity mockUnexpectedSeverity

-- | Sets the severity for expectations that were never met.  Defaults to
-- 'Error'.
setUnmetExpectationCheck :: (e1 <: es) => MockSource root resp e1 -> Severity -> Eff es ()
setUnmetExpectationCheck = setSeverity mockUnmetSeverity

setSeverity ::
  (e1 <: es) =>
  (MockState IO -> TVar Severity) ->
  MockSource root resp e1 ->
  Severity ->
  Eff es ()
setSeverity field (MockSource st io _) severity = effIO io $
  atomically $ runMockSTM st $ do
    state <- askState
    liftSTM $ writeTVar (field state) severity

-- | Describes the outstanding expectations, which is sometimes useful for
-- debugging test code.  The exact format is not specified.
describeExpectations :: (e1 <: es) => MockSource root resp e1 -> Eff es String
describeExpectations (MockSource st io _) =
  effIO io (describeExpectationsWith st)

-- | Verifies that all expectations are satisfied.  'runMock' does this at the
-- end of a test, so you rarely need it, but it's occasionally useful to check
-- expectations early.
verifyExpectations ::
  (HasCallStack, e1 <: es) => MockSource root resp e1 -> Eff es ()
verifyExpectations (MockSource st io _) = verifyState io st

verifyState :: (HasCallStack, e1 <: es) => IOE e1 -> MockState IO -> Eff es ()
verifyState io st = effIO io $ do
  outstanding <- atomically $ runMockSTM st $ do
    state <- askState
    expectSet <- liftSTM $ readTVar (mockExpectSet state)
    missingSev <- liftSTM $ readTVar (mockUnmetSeverity state)
    return $ case excess expectSet of
      ExpectNothing -> Nothing
      missing -> Just (missingSev, missing)
  case outstanding of
    Nothing -> return ()
    Just (sev, missing) ->
      reportFault sev $ "Unmet expectations:\n" ++ formatExpectSet missing
