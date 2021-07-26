{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | This module defines the 'Rule' type, which describes a matcher for methods
-- and a (possibly empty) list of responses to use for successive calls to
-- matching methods.  The 'Expectable' type class generalizes 'Rule', so that
-- you can specify a bare 'Matcher' or 'Action' in most situations where a
-- 'Rule' is needed but you don't want to provide a response.
module Test.HMock.Rule
  ( Rule,
    Expectable (..),
    (|->),
    (|=>),
    WholeMethodMatcher (SuchThat),
  )
where

import Test.HMock.Internal.Rule (Rule (..), WholeMethodMatcher (..))
import {-# SOURCE #-} Test.HMock.Internal.State (MockT)
import Test.HMock.Mockable (MockableBase (Action, Matcher))

-- | Class for things that can be expected.  This is includes 'Rule's, but also
-- bare 'Matcher's and 'Action's with no explicit response.
class Expectable cls name m r ex | ex -> cls name m r where
  -- | Converts an expectable to a Rule that means the same thing.
  toRule :: ex -> Rule cls name m r

-- | Attaches a response to an expectation.  This is a flexible response,
-- which can look at arguments, do things in the base monad, set up more
-- expectations, etc.  A matching 'Action' is passed to the response.
(|=>) ::
  Expectable cls name m r ex =>
  ex ->
  (Action cls name m r -> MockT m r) ->
  Rule cls name m r
e |=> r = m :=> (rs ++ [r]) where m :=> rs = toRule e

infixl 1 |=>

-- | Attaches a return value to an expectation.  This is more convenient than
-- '|=>' in the common case where you just want to return a known result.
-- @e '|->' r@ means the same thing as @e '|=>' 'const' ('return' r)@.
(|->) ::
  (Monad m, Expectable cls name m r ex) =>
  ex ->
  r ->
  Rule cls name m r
m |-> r = m |=> const (return r)

infixl 1 |->

instance Expectable cls name m r (Rule cls name m r) where
  toRule = id

instance Expectable cls name m r (Matcher cls name m r) where
  toRule m = JustMatcher m :=> []

instance Expectable cls name m r (WholeMethodMatcher cls name m r) where
  toRule m = m :=> []
