{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- | This module defines the desugaring from multi-response 'Rule's into
-- multiple steps.
module Test.HMock.Internal.Step where

import Data.Kind (Constraint, Type)
import Data.Maybe (listToMaybe)
import GHC.Stack (CallStack, callStack)
import GHC.TypeLits (Symbol)
import Test.HMock.ExpectContext (ExpectContext (..), MockableMethod)
import Test.HMock.Internal.ExpectSet (ExpectSet (..))
import Test.HMock.Internal.Rule
  ( Rule (..),
    WholeMethodMatcher (..),
    showWholeMatcher,
  )
import {-# SOURCE #-} Test.HMock.Internal.State (MockT)
import Test.HMock.Internal.Util (Located (..), locate, withLoc)
import Test.HMock.Mockable (MockableBase (..))
import Test.HMock.Multiplicity
  ( Multiplicity,
    anyMultiplicity,
    feasible,
    meetsMultiplicity,
  )
import Test.HMock.Rule (Expectable (toRule))

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
    WholeMethodMatcher cls name m r ->
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
    withLoc (showWholeMatcher Nothing m <$ l)

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
  | meetsMultiplicity mult 0 = ExpectEither ExpectNothing body
  | otherwise = body
  where
    body =
      ExpectSequence
        (ExpectStep (Step (locate callstack (m :-> Just r1))))
        (expandRepeatRule (mult - 1) callstack (m :=> (r2 : rs)))
expandRepeatRule mult callstack (m :=> rs) =
  ExpectConsecutive
    mult
    (ExpectStep (Step (locate callstack (m :-> listToMaybe rs))))

-- | Newtype wrapper to make the type of ExpectSet conform to the ExpectContext
-- class.  The "return type" a is a phantom.
newtype Expected m a = Expected {unwrapExpected :: ExpectSet (Step m)}

instance ExpectContext Expected where
  expect e = Expected (expandRule callStack (toRule e))
  expectN mult e = Expected (expandRepeatRule mult callStack (toRule e))
  expectAny e =
    Expected (expandRepeatRule anyMultiplicity callStack (toRule e))
  inSequence es = Expected (foldr1 ExpectSequence (map unwrapExpected es))
  inAnyOrder es = Expected (foldr1 ExpectInterleave (map unwrapExpected es))
  anyOf es = Expected (foldr1 ExpectEither (map unwrapExpected es))
  times mult e = Expected (ExpectMulti mult (unwrapExpected e))
  consecutiveTimes mult e =
    Expected (ExpectConsecutive mult (unwrapExpected e))
