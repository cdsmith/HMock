{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines the 'ExpectContext' class, whose members provide the
-- combinators for building the execution plan for your mocks.  Notably, there
-- is a t'Test.HMock.Classic.MockT.MockT' instance for 'ExpectContext', so you can use
-- these combinators to add expectations inside your tests that run in
-- t'Test.HMock.Classic.MockT.MockT', as well as nesting them in other combinators.
module Test.HMock.Classic.ExpectContext
  ( MockableMethod,
    ExpectContext (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Test.HMock.Classic.Internal
  ( Expectable (toRule),
    MockContext (fromMockSetup),
    MockSetup,
    MockT,
    MockableMethod,
    liftMockSTM,
    prepareRule,
  )
import Test.HMock.Internal.ExpectSet (ExpectSet (..))
import Test.HMock.Multiplicity (Multiplicity, anyMultiplicity)
import Test.HMock.Internal.State (expectThisSet)
import Test.HMock.Internal.Step (Step, expandRepeatRule, expandRule)

-- | Type class for contexts in which one can build expectations.  Notably, this
-- includes `Test.HMock.Classic.MockT.MockT`, which expects actions to be performed
-- during a test.
--
-- The methods of this class represent the user-facing API for build your
-- execution plan for mocks.
class ExpectContext (ctx :: (Type -> Type) -> Type -> Type) where
  -- | Creates an expectation that an action is performed once per given
  -- response (or exactly once if there is no response).
  --
  -- @
  -- 'Test.HMock.Classic.MockT.runMockT' '$' do
  --   'expect' '$'
  --     ReadFile "foo.txt"
  --       'Test.HMock.Classic.Rule.|->' "lorem ipsum"
  --       'Test.HMock.Classic.Rule.|->' "oops, the file changed out from under me!"
  --   callCodeUnderTest
  -- @
  --
  -- In this example, `readFile` must be called exactly twice by the tested
  -- code, and will return "lorem ipsum" the first time, but something different
  -- the second time.
  expect ::
    ( HasCallStack,
      MonadIO m,
      MockableMethod cls name m r,
      Expectable cls name m r expectable
    ) =>
    expectable ->
    ctx m ()

  -- | Creates an expectation that an action is performed some number of times.
  --
  -- @
  --   'Test.HMock.Classic.MockT.runMockT' '$' do
  --     'expect' '$' MakeList
  --     'expectN' ('Test.HMock.atLeast' 2) '$'
  --       CheckList "Cindy Lou Who" 'Test.HMock.Classic.Rule.|->' Nice
  --
  --     callCodeUnderTest
  -- @
  expectN ::
    ( HasCallStack,
      MonadIO m,
      MockableMethod cls name m r,
      Expectable cls name m r expectable
    ) =>
    -- | The number of times the action should be performed.
    Multiplicity ->
    -- | The action and its response.
    expectable ->
    ctx m ()

  -- | Specifies a response if a matching action is performed, but doesn't
  -- expect anything.  This is equivalent to @'expectN'
  -- 'Test.HMock.Classic.anyMultiplicity'@, but shorter.
  --
  -- In this example, the later use of 'expectAny' overrides earlier uses, but
  -- only for calls that match its conditions.
  --
  -- @
  --   'Test.HMock.Classic.MockT.runMockT' '$' do
  --     'expectAny' '$'
  --       ReadFile_ anything 'Test.HMock.Classic.Rule.|->' "tlhIngan maH!"
  --     'expectAny' '$'
  --       ReadFile "config.txt" 'Test.HMock.Classic.Rule.|->' "lang: klingon"
  --
  --     callCodeUnderTest
  -- @
  expectAny ::
    ( HasCallStack,
      MonadIO m,
      MockableMethod cls name m r,
      Expectable cls name m r expectable
    ) =>
    expectable ->
    ctx m ()

  -- | Creates a sequential expectation.  Other actions can still happen during
  -- the sequence, but these specific expectations must be met in this order.
  --
  -- @
  --   'inSequence'
  --     [ 'expect' '$' MoveForward,
  --       'expect' '$' TurnRight,
  --       'expect' '$' MoveForward
  --     ]
  -- @
  --
  -- Beware of using 'inSequence' too often.  It is appropriate when the
  -- property you are testing is that the order of effects is correct.  If
  -- that's not the purpose of the test, consider adding several independent
  -- expectations, instead.  This avoids over-asserting, and keeps your tests
  -- less brittle.
  --
  -- An empty list expects nothing.
  inSequence ::
    MonadIO m => (forall ctx'. ExpectContext ctx' => [ctx' m ()]) -> ctx m ()

  -- | Combines multiple expectations, which can occur in any order.  Most of
  -- the time, you can achieve the same thing by expecting each separately, but
  -- this can be combined in compound expectations to describe more complex
  -- ordering constraints.
  --
  -- If ambiguity checking is disabled, the choice is left-biased, so earlier
  -- options are preferred over ambiguous later options.
  --
  -- @
  --   'inSequence'
  --     [ 'inAnyOrder'
  --         [ 'expect' '$' AdjustMirrors,
  --           'expect' '$' FastenSeatBelt
  --         ],
  --       'expect' '$' StartCar
  --     ]
  -- @
  --
  -- An empty list expects nothing.
  inAnyOrder ::
    MonadIO m => (forall ctx'. ExpectContext ctx' => [ctx' m ()]) -> ctx m ()

  -- | Combines multiple expectations, requiring exactly one of them to occur.
  -- If ambiguity checking is disabled, the choice is left-biased, so earlier
  -- options are preferred over ambiguous later options.
  --
  -- @
  --   'anyOf'
  --     [ 'expect' $ ApplyForJob,
  --       'expect' $ ApplyForUniversity
  --     ]
  -- @
  --
  -- The list must not be empty, since a choice among no alternatives can never
  -- be satisfied.  @'anyOf' []@ is an error.
  anyOf ::
    (HasCallStack, MonadIO m) =>
    (forall ctx'. ExpectContext ctx' => [ctx' m ()]) ->
    ctx m ()

  -- | Creates a parent expectation that the child expectation will happen a
  -- certain number of times.  Unlike `expectN`, the child expectation can be
  -- arbitrarily complex and span multiple actions.  Also unlike 'expectN', each
  -- new execution will restart response sequences for rules with more than one
  -- response.
  --
  -- Different occurrences of the child can be interleaved.  If ambiguity
  -- checking is disabled, progressing on an existing occurrence is preferred
  -- over starting a new occurrence when it's ambiguous.
  times ::
    MonadIO m =>
    Multiplicity ->
    (forall ctx'. ExpectContext ctx' => ctx' m ()) ->
    ctx m ()

  -- | Creates a parent expectation that the child expectation will happen a
  -- certain number of times.  Unlike `expectN`, the child expectation can be
  -- arbitrarily complex and span multiple actions.  Also unlike 'expectN', each
  -- new execution will restart response sequences for rules with more than one
  -- response.
  --
  -- Different occurrences of the child must happen consecutively, with one
  -- finishing before the next begins.
  consecutiveTimes ::
    MonadIO m =>
    Multiplicity ->
    (forall ctx'. ExpectContext ctx' => ctx' m ()) ->
    ctx m ()

-- | An expectation under construction.  The "return type" @a@ is a phantom.
--
-- Building runs in t'MockSetup', because each rule's class setup has to run
-- and each response has to be closed over the environment before the engine
-- can see it.
newtype Expected m a = Expected
  {unwrapExpected :: MockSetup m (ExpectSet (Step m))}

instance ExpectContext Expected where
  expect e = Expected (expandRule Nothing callStack <$> prepareRule (toRule e))
  expectN mult e =
    Expected (expandRepeatRule Nothing mult callStack <$> prepareRule (toRule e))
  expectAny e =
    Expected
      ( expandRepeatRule Nothing anyMultiplicity callStack
          <$> prepareRule (toRule e)
      )
  inSequence es =
    Expected (foldr ExpectSequence ExpectNothing <$> mapM unwrapExpected es)
  inAnyOrder es =
    Expected (foldr ExpectInterleave ExpectNothing <$> mapM unwrapExpected es)
  anyOf es = Expected $ do
    sets <- mapM unwrapExpected es
    case sets of
      [] -> error emptyAnyOf
      _ -> return (foldr1 ExpectEither sets)
  times mult e = Expected (ExpectMulti mult <$> unwrapExpected e)
  consecutiveTimes mult e =
    Expected (ExpectConsecutive mult <$> unwrapExpected e)

-- | The error for @'anyOf' []@.
emptyAnyOf :: String
emptyAnyOf = "anyOf requires at least one alternative; anyOf [] can never be satisfied."

-- | Builds an expectation and installs it.
install :: Expected m () -> MockSetup m ()
install e = unwrapExpected e >>= liftMockSTM . expectThisSet

-- | This instance allows you to add expectations from t'MockSetup' actions.
-- This is an unusual thing to do.  Consider using
-- 'Test.HMock.Classic.Defaults.allowUnexpected', instead.
instance ExpectContext MockSetup where
  expect e = withFrozenCallStack $ install (expect e)
  expectN mult e = withFrozenCallStack $ install (expectN mult e)
  expectAny e = withFrozenCallStack $ install (expectAny e)
  inSequence es = withFrozenCallStack $ install (inSequence es)
  inAnyOrder es = withFrozenCallStack $ install (inAnyOrder es)
  anyOf es = withFrozenCallStack $ install (anyOf es)
  times mult es = withFrozenCallStack $ install (times mult es)
  consecutiveTimes mult es =
    withFrozenCallStack $ install (consecutiveTimes mult es)

-- | Expectations added during a test take effect immediately.
instance ExpectContext MockT where
  expect e = withFrozenCallStack $ fromMockSetup $ install (expect e)
  expectN mult e = withFrozenCallStack $ fromMockSetup $ install (expectN mult e)
  expectAny e = withFrozenCallStack $ fromMockSetup $ install (expectAny e)
  inSequence es = withFrozenCallStack $ fromMockSetup $ install (inSequence es)
  inAnyOrder es = withFrozenCallStack $ fromMockSetup $ install (inAnyOrder es)
  anyOf es = withFrozenCallStack $ fromMockSetup $ install (anyOf es)
  times mult es = withFrozenCallStack $ fromMockSetup $ install (times mult es)
  consecutiveTimes mult es =
    withFrozenCallStack $ fromMockSetup $ install (consecutiveTimes mult es)
