{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- | The 'Rule' type: a matcher for methods together with responses to use
-- for successive matching calls.
--
-- Responses here run in the base monad @m@.  Each front end has its own rule
-- type whose responses run in its test monad, and converts to this one when
-- an expectation is installed.
module Test.HMock.Internal.Rule where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Test.HMock.Mockable (MockableBase (..))

-- | A way to match an entire action, using conditions that might depend on the
-- relationship between arguments.
data WholeMethodMatcher cls name r where
  JustMatcher :: Matcher cls name r -> WholeMethodMatcher cls name r
  SuchThat ::
    Matcher cls name r ->
    (Action cls name r -> Bool) ->
    WholeMethodMatcher cls name r

-- | Displays a WholeMethodMatcher.  The predicate isn't showable, but we can at
-- least indicate whether there is one present.
showWholeMatcher ::
  MockableBase cls =>
  Maybe (Action cls name a) ->
  WholeMethodMatcher cls name b ->
  String
showWholeMatcher a (JustMatcher m) = showMatcher a m
showWholeMatcher a (m `SuchThat` _) =
  showMatcher a m ++ " (with whole method matcher)"

-- | A rule for matching a method and responding to it when it matches.
--
-- The method may be matched by providing either an 'Action' to match exactly,
-- or a 'Matcher'.
--
-- A 'Rule' may have zero or more responses, each running in the base monad
-- @m@.  If there are none, the default response for that action is used.  If
-- there is more than one, they are used in order, repeating the last one for
-- additional matches.
--
-- Each front end has its own rule type, with responses in its test monad and
-- operators for attaching them, and converts to this one when an expectation
-- is installed.
data
  Rule
    (cls :: k)
    (name :: Symbol)
    (m :: Type -> Type)
    (r :: Type)
  where
  (:=>) ::
    WholeMethodMatcher cls name r ->
    [Action cls name r -> m r] ->
    Rule cls name m r
