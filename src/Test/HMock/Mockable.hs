{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the 'MockableBase' class, which describes an interface
-- that HMock can mock: the actions it can perform, and how to match them.
--
-- You will typically derive 'MockableBase' with Template Haskell, since it's
-- mostly boilerplate.
module Test.HMock.Mockable
  ( MockableBase (..),
    ExactMatch (..),
    MockableMethod,
    MatchResult (..),
  )
where

import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, Symbol)

-- | The result of matching a @'Matcher' a@ with an @'Action' b@.  Because the
-- types should already guarantee that the methods match, all that's left is to
-- match arguments.
data MatchResult where
  -- | No match.  The arg is explanations of mismatch.
  NoMatch :: [(Int, String)] -> MatchResult
  -- | Match. Stores a witness to the equality of return types.
  Match :: MatchResult

-- | A base class for interfaces whose methods can be mocked.
--
-- The @cls@ index is the interface itself, at whatever kind it has; each
-- front end decides what counts as an interface.  (For an mtl-style class it
-- is the class applied to its monad, of kind 'Data.Kind.Constraint'; for a
-- Bluefin capability it is the capability, of kind @Effects -> Type@.)
--
-- You usually want to generate this instance with your front end's Template
-- Haskell, since it's just boilerplate.
class (Typeable k, Typeable cls) => MockableBase (cls :: k) where
  -- | An action that is performed.  This data type will have one constructor
  -- for each method.
  data Action cls :: Symbol -> Type -> Type

  -- | A specification for matching actions.  The actual arguments should be
  -- replaced with predicates.
  data Matcher cls :: Symbol -> Type -> Type

  -- | Gets a text description of an 'Action', for use in error messages.
  showAction :: Action cls name a -> String

  -- | Gets a text description of a 'Matcher', for use in error messages.
  showMatcher :: Maybe (Action cls name a) -> Matcher cls name b -> String

  -- | Attempts to match an 'Action' with a 'Matcher'.
  matchAction :: Matcher cls name a -> Action cls name a -> MatchResult

-- | Methods whose arguments can all be compared for equality, so that an
-- 'Action' can stand in for the 'Matcher' that matches it exactly.
--
-- This is generated alongside 'MockableBase' for each method whose
-- argument types have 'Eq' and 'Show' instances.  A method with more
-- complex arguments has no usable instance, and must be matched with a
-- 'Matcher' instead.
class ExactMatch (cls :: k) (name :: Symbol) (r :: Type) where
  -- | The 'Matcher' that matches exactly this action.
  exactMatcher :: Action cls name r -> Matcher cls name r

-- | All constraints needed to mock a method with the given interface,
-- name, and return type.
type MockableMethod (cls :: k) (name :: Symbol) (r :: Type) =
  (MockableBase cls, KnownSymbol name, Typeable r)
