{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the 'MockableBase' and 'Mockable' classes that are
-- needed to use an MTL-style type class with 'Test.HMock.MockT.MockT'.  You
-- will typically derive 'MockableBase' with Template Haskell, since it's mostly
-- boilerplate.  The 'Mockable' class adds a customizable setup method which you
-- can define yourself to add the right defaults for methods in the mocked
-- class.
module Test.HMock.Mockable
  ( Mockable (..),
    MockableBase (..),
    MatchResult (..),
  )
where

import Control.Monad.Trans (MonadIO)
import Data.Kind (Constraint, Type)
import Data.Typeable (Typeable)
import GHC.TypeLits (Symbol)
import {-# SOURCE #-} Test.HMock.Internal.State (MockSetup)

-- | The result of matching a @'Matcher' a@ with an @'Action' b@.  Because the
-- types should already guarantee that the methods match, all that's left is to
-- match arguments.
data MatchResult where
  -- | No match.  The arg is explanations of mismatch.
  NoMatch :: [(Int, String)] -> MatchResult
  -- | Match. Stores a witness to the equality of return types.
  Match :: MatchResult

-- | A base class for 'Monad' subclasses whose methods can be mocked.  You
-- usually want to generate this instance using 'Test.HMock.TH.makeMockable',
-- 'Test.HMock.TH.makeMockable', or 'Test.HMock.TH.makeMockableWithOptions',
-- since it's just boilerplate.
class (Typeable cls) => MockableBase (cls :: (Type -> Type) -> Constraint) where
  -- | An action that is performed.  This data type will have one constructor
  -- for each method.
  data Action cls :: Symbol -> (Type -> Type) -> Type -> Type

  -- | A specification for matching actions.  The actual arguments should be
  -- replaced with predicates.
  data Matcher cls :: Symbol -> (Type -> Type) -> Type -> Type

  -- | Gets a text description of an 'Action', for use in error messages.
  showAction :: Action cls name m a -> String

  -- | Gets a text description of a 'Matcher', for use in error messages.
  showMatcher :: Maybe (Action cls name m a) -> Matcher cls name m b -> String

  -- | Attempts to match an 'Action' with a 'Matcher'.
  matchAction :: Matcher cls name m a -> Action cls name m a -> MatchResult

-- | A class for 'Monad' subclasses whose methods can be mocked.  This class
-- augments 'MockableBase' with a setup method that is run before HMock touches
-- the 'Monad' subclass for the first time.  The default implementation does
-- nothing, but you can derive your own instances that add setup behavior.
class MockableBase cls => Mockable (cls :: (Type -> Type) -> Constraint) where
  -- | An action to run and set up defaults for this class.  The action will be
  -- run before HMock touches the class, either to add expectations or to
  -- delegate a method.
  --
  -- By default, unexpected actions throw errors, and actions with no explicit
  -- default always return the default value of their return type, or
  -- 'undefined' if there is none.  You can change this on a per-class or
  -- per-test basis.
  --
  -- * To change defaults on a per-class basis, you should use
  --   'Test.HMock.MockT.allowUnexpected' and/or 'Test.HMock.MockT.byDefault'
  --   to perform the setup you need here.
  -- * To change defaults on a per-test basis, you should use
  --   'Test.HMock.MockT.allowUnexpected' and/or 'Test.HMock.MockT.byDefault'
  --   directly from the test.
  setupMockable :: (MonadIO m, Typeable m) => proxy cls -> MockSetup m ()
  setupMockable _ = return ()
