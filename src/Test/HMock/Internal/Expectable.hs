{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Test.HMock.Internal.Expectable where

import Control.Monad.Trans (MonadIO)
import Data.Kind (Constraint, Type)
import Data.Typeable
import GHC.Stack (CallStack, HasCallStack, callStack)
import GHC.TypeLits (KnownSymbol, Symbol)
import Test.HMock.Internal.ExpectSet
import {-# SOURCE #-} Test.HMock.Internal.MockT
import Test.HMock.Internal.Mockable
import Test.HMock.Internal.Multiplicity
import Test.HMock.Internal.Util (Located (Loc), locate, withLoc)

-- | Something that can be expected.  This type class covers a number of cases:
--
--   * Expecting an exact 'Action'.
--   * Expecting anything that matches a 'Matcher'.
--   * Adding a return value (with '|->') or response (with '|=>').
class Expectable cls name m r e | e -> cls name m r where
  toRule :: e -> Rule cls name m r

-- | A rule for matching a method and responding to it when it matches.
--
-- The method may be matched by providing either an 'Action' to match exactly,
-- or a 'Matcher'.  Exact matching is only available when all method arguments
--
--
-- A 'Rule' may have zero or more responses, which are attached using '|->' and
-- '|=>'.  If there are no responses for a 'Rule', then there must be a default
-- response for that action, and it is used.  If more than one response is
-- added, the rule will perform the responses in order, repeating the last
-- response if there are additional matches.
--
-- Example:
--
-- @
-- 'expect' $
--   GetLine_ 'Test.HMock.anything'
--     '|->' "hello"
--     '|=>' \(GetLine prompt) -> "The prompt was " ++ prompt
--     '|->' "quit"
-- @
data
  Rule
    (cls :: (Type -> Type) -> Constraint)
    (name :: Symbol)
    (m :: Type -> Type)
    (r :: Type)
  where
  (:=>) ::
    Matcher cls name m r ->
    [Action cls name m r -> MockT m r] ->
    Rule cls name m r

-- | Attaches a response to an expectation.  This is a very flexible response,
-- which can look at arguments, do things in the base monad, set up more
-- expectations, etc.  The matching 'Action' is passed to the response, and is
-- guaranteed to be a match so it's fine to just pattern match on the correct
-- method.
(|=>) ::
  Expectable cls name m r expectable =>
  expectable ->
  (Action cls name m r -> MockT m r) ->
  Rule cls name m r
e |=> r = m :=> (rs ++ [r]) where m :=> rs = toRule e

infixl 1 |=>

-- | Attaches a return value to an expectation.  This is more convenient than
-- '|=>' in the common case where you just want to return a known result.
-- @e '|->' r@ means the same thing as @e '|=>' 'const' ('return' r)@.
(|->) ::
  (Monad m, Expectable cls name m r expectable) =>
  expectable ->
  r ->
  Rule cls name m r
m |-> r = m |=> const (return r)

infixl 1 |->

instance Expectable cls name m r (Rule cls name m r) where
  toRule = id

instance Expectable cls name m r (Matcher cls name m r) where
  toRule m = m :=> []

-- | A single step of an expectation.
data Step m where
  Step :: MockableMethod cls name m r => Located (Rule cls name m r) -> Step m

instance Show (Step m) where
  show (Step l@(Loc _ (m :=> _))) =
    withLoc (showMatcher Nothing m <$ l)

instance Steppable (Step m) where
  nextStep (Step (Loc l (m :=> (_ : r : rs)))) = Step (Loc l (m :=> (r : rs)))
  nextStep other = other

-- | Type class for contexts in which it makes sense to express an expectation.
-- Notably, this includes `MockT`, which expects actions to be performed during
-- a test.
class ExpectContext (t :: (Type -> Type) -> Type -> Type) where
  fromExpectSet :: MonadIO m => ExpectSet (Step m) -> t m ()

newtype Expected m a = Expected {unwrapExpected :: ExpectSet (Step m)}

instance ExpectContext Expected where
  fromExpectSet = Expected

-- | All constraints needed to mock a method with the given class, name, base
-- monad, and return type.
type MockableMethod
  (cls :: (Type -> Type) -> Constraint)
  (name :: Symbol)
  (m :: Type -> Type)
  (r :: Type) =
  (Mockable cls, Typeable m, KnownSymbol name, Typeable r)

makeExpect ::
  (Expectable cls name m r expectable, MockableMethod cls name m r) =>
  CallStack ->
  Multiplicity ->
  expectable ->
  ExpectSet (Step m)
makeExpect cs mult e = Expect mult (Step (locate cs (toRule e)))

-- | Creates an expectation that an action is performed once per given
-- response (or exactly once if there is no response).
--
-- @
-- 'runMockT' '$' do
--   'expect' '$'
--     ReadFile "foo.txt"
--       '|->' "lorem ipsum"
--       '|->' "oops, the file changed out from under me!"
--   callCodeUnderTest
-- @
--
-- In this example, `readFile` must be called exactly twice by the tested code,
-- and will return "lorem ipsum" the first time, but something different the
-- second time.
expect ::
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r,
    Expectable cls name m r expectable,
    ExpectContext ctx
  ) =>
  expectable ->
  ctx m ()
expect e = fromExpectSet (makeExpect callStack (exactly (max 1 (length rs))) e)
  where
    (_ :=> rs) = toRule e

-- | Creates an expectation that an action is performed some number of times.
--
-- @
--   'runMockT' '$' do
--     'expect' '$' MakeList '|->' ()
--     'expectN' ('Test.HMock.atLeast' 2) '$'
--       CheckList "Cindy Lou Who" '|->' "nice"
--
--     callCodeUnderTest
-- @
expectN ::
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r,
    Expectable cls name m r expectable,
    ExpectContext ctx
  ) =>
  -- | The number of times the action should be performed.
  Multiplicity ->
  -- | The action and its response.
  expectable ->
  ctx m ()
expectN mult e
  | Multiplicity _ (Just n) <- mult,
    length rs > n =
    error "Too many responses for the specified multiplicity"
  | otherwise = fromExpectSet (makeExpect callStack mult e)
  where
    (_ :=> rs) = toRule e

-- | Specifies a response if a matching action is performed, but doesn't expect
-- anything.  This is equivalent to @'expectN' 'anyMultiplicity'@, but shorter.
--
-- In this example, the later use of 'whenever' overrides earlier uses, but only
-- for calls that match its conditions.
--
-- @
--   'runMockT' '$' do
--     'whenever' '$' ReadFile_ anything '|->' "tlhIngan maH!"
--     'whenever' '$' ReadFile "config.txt" '|->' "lang: klingon"
--
--     callCodeUnderTest
-- @
expectAny ::
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r,
    Expectable cls name m r expectable,
    ExpectContext ctx
  ) =>
  expectable ->
  ctx m ()
expectAny = fromExpectSet . makeExpect callStack anyMultiplicity

-- | Creates a sequential expectation.  Other actions can still happen during
-- the sequence, but these specific expectations must be met in this order.
--
-- @
--   'inSequence'
--     [ 'expect' '$' MoveForward '|->' (),
--       'expect' '$' TurnRight '|->' (),
--       'expect' '$' MoveForward '|->' ()
--     ]
-- @
--
-- Beware of using 'inSequence' too often.  It is appropriate when the property
-- you are testing is that the order of effects is correct.  If that's not the
-- purpose of the test, consider adding several independent expectations,
-- instead.  This avoids over-asserting, and keeps your tests less brittle.
inSequence ::
  (MonadIO m, ExpectContext ctx) =>
  (forall ctx'. ExpectContext ctx' => [ctx' m ()]) ->
  ctx m ()
inSequence es = fromExpectSet (ExpectMulti InOrder (map unwrapExpected es))

-- | Combines multiple expectations, which can occur in any order.  Most of the
-- time, you can achieve the same thing by expecting each separately, but this
-- can be combined with 'inSequence' to describe more complex ordering
-- constraints, such as:
--
-- @
--   'inSequence'
--     [ 'inAnyOrder'
--         [ 'expect' '$' adjustMirrors '|->' (),
--           'expect' '$' fastenSeatBelt '|->' ()
--         ],
--       'expect' '$' startCar '|->' ()
--     ]
-- @
inAnyOrder ::
  (MonadIO m, ExpectContext ctx) =>
  (forall ctx'. ExpectContext ctx' => [ctx' m ()]) ->
  ctx m ()
inAnyOrder es = fromExpectSet (ExpectMulti AnyOrder (map unwrapExpected es))