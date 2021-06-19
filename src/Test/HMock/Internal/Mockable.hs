{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.HMock.Internal.Mockable where

import Control.Monad.Trans (MonadIO)
import Data.Kind (Constraint, Type)
import Data.Typeable (Typeable)
import GHC.TypeLits (Symbol)
import {-# SOURCE #-} Test.HMock.Internal.MockT (MockT)

-- | The result of matching a @'Matcher' a@ with an @'Action' b@.  Because the
-- types should already guarantee that the methods match, all that's left is to
-- match arguments.
data MatchResult where
  -- | No match.  The int is the number of arguments that don't match.
  NoMatch :: Int -> MatchResult
  -- | Match. Stores a witness to the equality of return types.
  Match :: MatchResult

-- | A class for 'Monad' subclasses whose methods can be mocked.  You usually
-- want to generate this instance using 'Test.HMock.TH.makeMockable' or
-- 'Test.HMock.TH.deriveMockable', because it's just a lot of boilerplate.
class
  (Typeable cls, MockableSetup cls) =>
  Mockable (cls :: (Type -> Type) -> Constraint)
  where
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

-- | A class that can be used to set up default behaviors for a 'Mockable'
-- class.  There is a default instance that does nothing, but you can derive
-- your own instances that overlap that one and add setup behavior.
class MockableSetup (cls :: (Type -> Type) -> Constraint) where
  -- An action to run and set up defaults for this class.  The action will be
  -- run before HMock touches the class, either to add expectations or to
  -- delegate a method.
  setupMockable :: (MonadIO m, Typeable m) => proxy cls -> MockT m ()
  setupMockable _ = return ()

instance {-# OVERLAPPABLE #-} Mockable cls => MockableSetup cls
