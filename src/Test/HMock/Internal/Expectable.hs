{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.HMock.Internal.Expectable where

import Control.Monad.Trans (MonadIO)
import Data.Kind (Constraint, Type)
import Data.Maybe (listToMaybe)
import Data.Typeable
import GHC.Stack (CallStack, HasCallStack, callStack)
import GHC.TypeLits (KnownSymbol, Symbol)
import Test.HMock.Internal.ExpectSet
import {-# SOURCE #-} Test.HMock.Internal.MockT
import Test.HMock.Internal.Mockable
import Test.HMock.Internal.Multiplicity
import Test.HMock.Internal.Util (Located (Loc), locate, withLoc)

-- | Class for  things that can be expected.  Covers a number of cases:
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

-- | All constraints needed to mock a method with the given class, name, base
-- monad, and return type.
type MockableMethod
  (cls :: (Type -> Type) -> Constraint)
  (name :: Symbol)
  (m :: Type -> Type)
  (r :: Type) =
  (Mockable cls, Typeable m, KnownSymbol name, Typeable r)

-- | A Rule that contains only a single response.  This is the target for
-- desugaring the multi-response rule format.
data
  SingleRule
    (cls :: (Type -> Type) -> Constraint)
    (name :: Symbol)
    (m :: Type -> Type)
    (r :: Type)
  where
  (:->) ::
    Matcher cls name m r ->
    Maybe (Action cls name m r -> MockT m r) ->
    SingleRule cls name m r

-- | A single step of an expectation.
data Step m where
  Step ::
    MockableMethod cls name m r =>
    Located (SingleRule cls name m r) ->
    Step m

instance Show (Step m) where
  show (Step l@(Loc _ (m :-> _))) =
    withLoc (showMatcher Nothing m <$ l)

-- | Expands a Rule into an expectation.  The expected multiplicity will be one
-- if there are no responses; otherwise one call is expected per response.
expandRule ::
  MockableMethod cls name m r =>
  CallStack ->
  Rule cls name m r ->
  ExpectSet (Step m)
expandRule callstack (m :=> []) =
  ExpectStep (Step (locate callstack (m :-> Nothing)))
expandRule callstack (m :=> rs) =
  foldr1
    ExpectSequence
    (map (ExpectStep . Step . locate callstack . (m :->) . Just) rs)

-- | Expands a Rule into an expectation, given a target multiplicity.  It is an
-- error if there are too many responses for the multiplicity.  If there are
-- too few responses, the last response will be repeated.
expandRepeatRule ::
  MockableMethod cls name m r =>
  Multiplicity ->
  CallStack ->
  Rule cls name m r ->
  ExpectSet (Step m)
expandRepeatRule mult _ (_ :=> rs)
  | not (feasible (mult - fromIntegral (length rs))) =
    error $
      show (length rs)
        ++ " responses is too many for multiplicity "
        ++ show mult
expandRepeatRule mult callstack (m :=> (r1 : r2 : rs))
  | exhaustable mult = ExpectEither ExpectNothing body
  | otherwise = body
  where body = ExpectSequence
          (ExpectStep (Step (locate callstack (m :-> Just r1))))
          (expandRepeatRule (mult - 1) callstack (m :=> (r2 : rs)))
expandRepeatRule mult callstack (m :=> rs) =
  ExpectConsecutive
    mult
    (ExpectStep (Step (locate callstack (m :-> listToMaybe rs))))

-- | Type class for contexts in which it makes sense to express an expectation.
-- Notably, this includes `MockT`, which expects actions to be performed during
-- a test.
class ExpectContext (t :: (Type -> Type) -> Type -> Type) where
  fromExpectSet :: MonadIO m => ExpectSet (Step m) -> t m ()

newtype Expected m a = Expected {unwrapExpected :: ExpectSet (Step m)}

instance ExpectContext Expected where
  fromExpectSet = Expected

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
expect e = fromExpectSet (expandRule callStack (toRule e))

-- | Creates an expectation that an action is performed some number of times.
--
-- @
--   'runMockT' '$' do
--     'expect' '$' MakeList
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
expectN mult e = fromExpectSet (expandRepeatRule mult callStack (toRule e))

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
expectAny e =
  fromExpectSet (expandRepeatRule anyMultiplicity callStack (toRule e))

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
-- Beware of using 'inSequence' too often.  It is appropriate when the property
-- you are testing is that the order of effects is correct.  If that's not the
-- purpose of the test, consider adding several independent expectations,
-- instead.  This avoids over-asserting, and keeps your tests less brittle.
inSequence ::
  (MonadIO m, ExpectContext ctx) =>
  (forall ctx'. ExpectContext ctx' => [ctx' m ()]) ->
  ctx m ()
inSequence es = fromExpectSet (foldr1 ExpectSequence (map unwrapExpected es))

-- | Combines multiple expectations, which can occur in any order.  Most of the
-- time, you can achieve the same thing by expecting each separately, but this
-- can be combined in complex expectations to describe more complex ordering
-- constraints.
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
  (MonadIO m, ExpectContext ctx) =>
  (forall ctx'. ExpectContext ctx' => [ctx' m ()]) ->
  ctx m ()
inAnyOrder es = fromExpectSet (foldr1 ExpectInterleave (map unwrapExpected es))

-- | Combines multiple expectations, requiring exactly one of them to occur.
--
-- @
--   'anyOf'
--     [ 'expect' $ ApplyForJob,
--       'expect' $ ApplyForUniversity
--     ]
-- @
anyOf ::
  (MonadIO m, ExpectContext ctx) =>
  (forall ctx'. ExpectContext ctx' => [ctx' m ()]) ->
  ctx m ()
anyOf es = fromExpectSet (foldr1 ExpectEither (map unwrapExpected es))

-- | Creates a parent expectation that the child expectation will happen a
-- certain number of times.  Unlike `expectN`, the child expectation can be
-- arbitrarily complex and span multiple actions.  Also unlike 'expectN', each
-- new execution will restart response sequences for rules with more than one
-- response.
--
-- Different occurrences of the child can be interleaved.  In case of ambiguity,
-- progressing on an existing occurrence is preferred over starting a new
-- occurrence.
times ::
  (MonadIO m, ExpectContext ctx) =>
  Multiplicity ->
  (forall ctx'. ExpectContext ctx' => ctx' m ()) ->
  ctx m ()
times mult e = fromExpectSet (ExpectMulti mult (unwrapExpected e))

-- | Creates a parent expectation that the child expectation will happen a
-- certain number of times.  Unlike `expectN`, the child expectation can be
-- arbitrarily complex and span multiple actions.  Also unlike 'expectN', each
-- new execution will restart response sequences for rules with more than one
-- response.
--
-- Different occurrences of the child must happen consecutively, with one
-- finishing before the next begins.
consecutiveTimes ::
  (MonadIO m, ExpectContext ctx) =>
  Multiplicity ->
  (forall ctx'. ExpectContext ctx' => ctx' m ()) ->
  ctx m ()
consecutiveTimes mult e =
  fromExpectSet (ExpectConsecutive mult (unwrapExpected e))
