{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | This module defines the 'ExpectContext' class, whose members provide the
-- combinators for building the execution plan for your mocks.  Notably, there
-- is a 'Test.HMock.MockT.MockT' instance for 'ExpectContext', so you can use
-- these combinators to add expectations inside your tests that run in
-- 'Test.HMock.MockT.MockT', as well as nesting them in other combinators.
module Test.HMock.ExpectContext
  ( MockableMethod,
    ExpectContext (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Constraint, Type)
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownSymbol, Symbol)
import Test.HMock.Mockable (Mockable)
import Test.HMock.Multiplicity (Multiplicity)
import Test.HMock.Rule (Expectable)

-- | All constraints needed to mock a method with the given class, name, base
-- monad, and return type.
type MockableMethod
  (cls :: (Type -> Type) -> Constraint)
  (name :: Symbol)
  (m :: Type -> Type)
  (r :: Type) =
  (Mockable cls, Typeable m, KnownSymbol name, Typeable r)

-- | Type class for contexts in which one can build expectations.  Notably, this
-- includes `Test.HMock.MockT.MockT`, which expects actions to be performed
-- during a test.
--
-- The methods of this class represent the user-facing API for build your
-- execution plan for mocks.
class ExpectContext (ctx :: (Type -> Type) -> Type -> Type) where
  -- | Creates an expectation that an action is performed once per given
  -- response (or exactly once if there is no response).
  --
  -- @
  -- 'Test.HMock.MockT.runMockT' '$' do
  --   'expect' '$'
  --     ReadFile "foo.txt"
  --       'Test.HMock.Rule.|->' "lorem ipsum"
  --       'Test.HMock.Rule.|->' "oops, the file changed out from under me!"
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
  --   'Test.HMock.MockT.runMockT' '$' do
  --     'expect' '$' MakeList
  --     'expectN' ('Test.HMock.atLeast' 2) '$'
  --       CheckList "Cindy Lou Who" 'Test.HMock.Rule.|->' Nice
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
  -- 'Test.HMock.Multiplicity.anyMultiplicity'@, but shorter.
  --
  -- In this example, the later use of 'expectAny' overrides earlier uses, but
  -- only for calls that match its conditions.
  --
  -- @
  --   'Test.HMock.MockT.runMockT' '$' do
  --     'expectAny' '$'
  --       ReadFile_ anything 'Test.HMock.Rule.|->' "tlhIngan maH!"
  --     'expectAny' '$'
  --       ReadFile "config.txt" 'Test.HMock.Rule.|->' "lang: klingon"
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
  anyOf ::
    MonadIO m => (forall ctx'. ExpectContext ctx' => [ctx' m ()]) -> ctx m ()

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
