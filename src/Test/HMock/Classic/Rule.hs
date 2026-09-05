{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the 'Rule' type, which describes a matcher for methods
-- and a (possibly empty) list of responses to use for successive calls to
-- matching methods.  The 'Expectable' type class generalizes 'Rule', so that
-- you can specify a bare 'Test.HMock.Classic.Matcher' or 'Action' in most
-- situations where a 'Rule' is needed but you don't want to provide a
-- response.
module Test.HMock.Classic.Rule
  ( Rule,
    Expectable (..),
    MockableMethod,
    (|->),
    (|=>),
    WholeMethodMatcher (SuchThat),
  )
where

import Test.HMock.Classic.Internal
  ( Expectable (..),
    MockT,
    MockableMethod,
    Rule (..),
  )
import Test.HMock.Mockable (MockableBase (Action))
import Test.HMock.Internal.Rule (WholeMethodMatcher (SuchThat))

-- | Attaches a response to an expectation.  This is a flexible response,
-- which can look at arguments, perform effects, set up more expectations,
-- etc.  A matching 'Action' is passed to the response, which runs in
-- 'Test.HMock.Classic.MockT.MockT', in the block where the rule was installed.
(|=>) ::
  Expectable cls name m r ex =>
  ex ->
  (Action (cls m) name r -> MockT m r) ->
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
