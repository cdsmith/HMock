{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- | This module defines the desugaring from multi-response 'Rule's into
-- multiple steps.
module Test.HMock.Internal.Step where

import Data.Kind (Type)
import Data.Maybe (listToMaybe)
import GHC.Stack (CallStack)
import GHC.TypeLits (Symbol)
import Test.HMock.Internal.ExpectSet (ExpectSet (..))
import Test.HMock.Internal.Rule
  ( Rule (..),
    WholeMethodMatcher (..),
    showWholeMatcher,
  )
import Test.HMock.Internal.Util
  ( InstanceId,
    Located (..),
    locate,
    showInstanceId,
    withLoc,
  )
import Test.HMock.Mockable (MockableBase (..), MockableMethod)
import Test.HMock.Multiplicity
  ( Multiplicity,
    feasible,
    meetsMultiplicity,
  )

-- | A Rule that contains only a single response.  This is the target for
-- desugaring the multi-response rule format.
data
  SingleRule
    (cls :: k)
    (name :: Symbol)
    (m :: Type -> Type)
    (r :: Type)
  where
  (:->) ::
    WholeMethodMatcher cls name r ->
    Maybe (Action cls name r -> m r) ->
    SingleRule cls name m r

-- | A single step of an expectation, together with the instance it applies
-- to.  'Nothing' means it applies to every instance.
data Step m where
  Step ::
    MockableMethod cls name r =>
    Maybe InstanceId ->
    Located (SingleRule cls name m r) ->
    Step m

instance Show (Step m) where
  show (Step instId l@(Loc _ (m :-> _))) =
    withLoc (showWholeMatcher Nothing m <$ l) ++ showInstanceId instId

-- | Expands a Rule into an expectation.  The expected multiplicity will be one
-- if there are no responses; otherwise one call is expected per response.
expandRule ::
  MockableMethod cls name r =>
  Maybe InstanceId ->
  CallStack ->
  Rule cls name m r ->
  ExpectSet (Step m)
expandRule instId callstack (m :=> []) =
  ExpectStep (Step instId (locate callstack (m :-> Nothing)))
expandRule instId callstack (m :=> rs) =
  foldr1
    ExpectSequence
    (map (ExpectStep . Step instId . locate callstack . (m :->) . Just) rs)

-- | Expands a Rule into an expectation, given a target multiplicity.  It is an
-- error if there are too many responses for the multiplicity.  If there are
-- too few responses, the last response will be repeated.
expandRepeatRule ::
  MockableMethod cls name r =>
  Maybe InstanceId ->
  Multiplicity ->
  CallStack ->
  Rule cls name m r ->
  ExpectSet (Step m)
expandRepeatRule _ mult _ (_ :=> rs)
  | not (feasible (mult - fromIntegral (length rs))) =
    error $
      show (length rs)
        ++ " responses is too many for multiplicity "
        ++ show mult
expandRepeatRule instId mult callstack (m :=> (r1 : r2 : rs))
  | meetsMultiplicity mult 0 = ExpectEither ExpectNothing body
  | otherwise = body
  where
    body =
      ExpectSequence
        (ExpectStep (Step instId (locate callstack (m :-> Just r1))))
        (expandRepeatRule instId (mult - 1) callstack (m :=> (r2 : rs)))
expandRepeatRule instId mult callstack (m :=> rs) =
  ExpectConsecutive
    mult
    (ExpectStep (Step instId (locate callstack (m :-> listToMaybe rs))))
