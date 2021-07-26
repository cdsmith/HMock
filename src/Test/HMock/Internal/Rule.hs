{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- | Internal module to define 'Rule', so that its constructor can be visible
-- to other implementation code.
module Test.HMock.Internal.Rule where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (Symbol)
import {-# SOURCE #-} Test.HMock.Internal.State (MockT)
import Test.HMock.Mockable (MockableBase (..))

-- | A way to match an entire action, using conditions that might depend on the
-- relationship between arguments.
data WholeMethodMatcher cls name m r where
  JustMatcher :: Matcher cls name m r -> WholeMethodMatcher cls name m r
  SuchThat ::
    Matcher cls name m r ->
    (Action cls name m r -> Bool) ->
    WholeMethodMatcher cls name m r

-- | Displays a WholeMethodMatcher.  The predicate isn't showable, but we can at
-- least indicate whether there is one present.
showWholeMatcher ::
  MockableBase cls =>
  Maybe (Action cls name m a) ->
  WholeMethodMatcher cls name m b ->
  String
showWholeMatcher a (JustMatcher m) = showMatcher a m
showWholeMatcher a (m `SuchThat` _) =
  showMatcher a m ++ " (with whole method matcher)"

-- | A rule for matching a method and responding to it when it matches.
--
-- The method may be matched by providing either an 'Action' to match exactly,
-- or a 'Matcher'.  Exact matching is only available when all method arguments
--
-- A 'Rule' may have zero or more responses, which are attached using
-- 'Test.HMock.Rule.|->' and 'Test.HMock.Rule.|=>'.  If there are no responses
-- for a 'Rule', then there must be a default response for that action, and it
-- is used.  If more than one response is added, the rule will perform the
-- responses in order, repeating the last response if there are additional
-- matches.
--
-- Example:
--
-- @
-- 'Test.HMock.ExpectContext.expect' $
--   GetLine_ 'Test.HMock.anything'
--     'Test.HMock.Rule.|->' "hello"
--     'Test.HMock.Rule.|=>' \(GetLine prompt) -> "The prompt was " ++ prompt
--     'Test.HMock.Rule.|->' "quit"
-- @
data
  Rule
    (cls :: (Type -> Type) -> Constraint)
    (name :: Symbol)
    (m :: Type -> Type)
    (r :: Type)
  where
  (:=>) ::
    WholeMethodMatcher cls name m r ->
    [Action cls name m r -> MockT m r] ->
    Rule cls name m r
