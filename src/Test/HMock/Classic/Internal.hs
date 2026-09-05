{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The definitions behind "Test.HMock.Classic": the test and setup monads,
-- the 'Mockable' class, and the 'Rule' type, together with the plumbing the
-- public modules share.
module Test.HMock.Classic.Internal where

import Control.Monad (unless)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Reader
  ( MonadReader (..),
    ReaderT (..),
    mapReaderT,
    runReaderT,
  )
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer (MonadWriter)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.TypeLits (Symbol)
import Test.HMock.Mockable (ExactMatch (..), MockableBase (..))
import qualified Test.HMock.Mockable as Internal
import Test.HMock.Internal.Rule (WholeMethodMatcher (..))
import qualified Test.HMock.Internal.Rule as Internal
import Test.HMock.Internal.State
  ( MockSTM,
    MockState (..),
    rootState,
    runMockSTM,
  )
import UnliftIO (MonadUnliftIO, STM, TVar, atomically, modifyTVar, readTVar)

-- | Everything a t'MockT' or t'MockSetup' action can see: the mock state, and
-- the classes whose 'setupMockable' has already run.  A nested block gets its
-- own state but shares the class registry with its parent.
data MockEnv m = MockEnv
  { mockEnvState :: MockState m,
    mockEnvClasses :: TVar (Set TypeRep)
  }

-- | Monad transformer for running mocks.
newtype MockT m a where
  MockT :: {unMockT :: ReaderT (MockEnv m) m a} -> MockT m a
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadIO,
      MonadState s,
      MonadWriter w,
      MonadRWS r w s,
      MonadError e,
      MonadCont,
      MonadBase b,
      MonadCatch,
      MonadMask,
      MonadThrow,
      MonadUnliftIO
    )

instance MonadTrans MockT where
  lift = MockT . lift

-- | Applies a function to the base monad of t'MockT'.
mapMockT :: (m a -> m b) -> MockT m a -> MockT m b
mapMockT f = MockT . mapReaderT f . unMockT

instance MonadReader r m => MonadReader r (MockT m) where
  ask = lift ask
  local = mapMockT . local
  reader = lift . reader

-- | Builds a t'MockT' action out of one that takes the environment explicitly.
withMockEnv :: (MockEnv m -> m a) -> MockT m a
withMockEnv = MockT . ReaderT

-- | Runs a t'MockT' action in a given environment.
runMockTWith :: MockT m a -> MockEnv m -> m a
runMockTWith = runReaderT . unMockT

-- | Monad for setting up a mockable class.  Note that even though the type
-- looks that way, this is *not* a monad transformer.  It's a very restricted
-- environment that can only be used to set up defaults for a class.
newtype MockSetup m a where
  MockSetup :: {unMockSetup :: ReaderT (MockEnv m) STM a} -> MockSetup m a
  deriving (Functor, Applicative, Monad)

-- | The environment a t'MockSetup' action is running in.
askMockEnv :: MockSetup m (MockEnv m)
askMockEnv = MockSetup ask

-- | Runs a transaction on the mock state from t'MockSetup'.
liftMockSTM :: MockSTM m a -> MockSetup m a
liftMockSTM m = MockSetup $ ReaderT $ \env -> runMockSTM (mockEnvState env) m

-- | Runs a t'MockSetup' action, atomically, in a given environment.
runMockSetupWith :: MonadIO m => MockSetup m a -> MockEnv m -> m a
runMockSetupWith m env = atomically (runReaderT (unMockSetup m) env)

-- | This type class defines a shared API between the t'MockT' and t'MockSetup'
-- monads.
class MockContext ctx where
  -- | Runs a t'MockSetup' action in this monad.
  fromMockSetup :: MonadIO m => MockSetup m a -> ctx m a

instance MockContext MockSetup where
  fromMockSetup = id

instance MockContext MockT where
  fromMockSetup m = withMockEnv (runMockSetupWith m)

-- | An mtl-style type class whose methods can be mocked.  The instance is for
-- the class itself, not applied to a monad: @instance Mockable MonadFoo@.
--
-- This adds a setup action that is run before HMock touches the class for the
-- first time.  The default implementation does nothing, but you can write your
-- own instance that adds setup behavior.
class Typeable cls => Mockable (cls :: (Type -> Type) -> Constraint) where
  -- | An action to run and set up defaults for this class.  It will be run
  -- before HMock touches the class, either to add expectations or to delegate
  -- a method.
  --
  -- By default, unexpected actions throw errors, and actions with no explicit
  -- default always return the default value of their return type, or
  -- 'undefined' if there is none.  You can change this per class or per test.
  --
  -- * To change defaults for the whole class, use
  --   'Test.HMock.Classic.Defaults.allowUnexpected' and\/or
  --   'Test.HMock.Classic.Defaults.byDefault' to perform the setup you need
  --   here.
  -- * To change defaults for one test, use them directly from the test.
  setupMockable :: (MonadIO m, Typeable m) => proxy cls -> MockSetup m ()
  setupMockable _ = return ()

-- | Runs class initialization for a 'Mockable' class, if it hasn't been run
-- yet.  Setup applies to the root state, so that defaults established from a
-- nested block outlive it.
initClassIfNeeded ::
  forall cls m proxy.
  (Mockable cls, Typeable m, MonadIO m) =>
  proxy cls ->
  MockSetup m ()
initClassIfNeeded proxy = MockSetup $ local atRoot $ unMockSetup $ do
  classes <- mockEnvClasses <$> askMockEnv
  done <- MockSetup (lift (readTVar classes))
  unless (Set.member t done) $ do
    MockSetup (lift (modifyTVar classes (Set.insert t)))
    setupMockable (Proxy :: Proxy cls)
  where
    t = typeRep proxy
    atRoot env = env {mockEnvState = rootState (mockEnvState env)}

-- | All constraints needed to mock a method of the given class, with the given
-- name, base monad, and return type.
type MockableMethod
  (cls :: (Type -> Type) -> Constraint)
  (name :: Symbol)
  (m :: Type -> Type)
  (r :: Type) =
  (Mockable cls, Internal.MockableMethod (cls m) name r, Typeable m)

-- | A rule for matching a method and responding to it when it matches.
--
-- The method may be matched by providing either an 'Action' to match exactly,
-- or a 'Matcher'.  Exact matching is only available when all method arguments
-- have 'Eq' and 'Show' instances.
--
-- A 'Rule' may have zero or more responses, which are attached using 'Test.HMock.Classic.Rule.|->'
-- and 'Test.HMock.Classic.Rule.|=>'.  If there are no responses for a 'Rule', then there must be a
-- default response for that action, and it is used.  If more than one
-- response is added, the rule will perform the responses in order, repeating
-- the last response if there are additional matches.
--
-- Example:
--
-- @
-- 'Test.HMock.Classic.ExpectContext.expect' $
--   GetLine_ 'Test.Predicates.anything'
--     'Test.HMock.Classic.Rule.|->' "hello"
--     'Test.HMock.Classic.Rule.|=>' \(GetLine prompt) -> "The prompt was " ++ prompt
--     'Test.HMock.Classic.Rule.|->' "quit"
-- @
data
  Rule
    (cls :: (Type -> Type) -> Constraint)
    (name :: Symbol)
    (m :: Type -> Type)
    (r :: Type)
  where
  (:=>) ::
    WholeMethodMatcher (cls m) name r ->
    [Action (cls m) name r -> MockT m r] ->
    Rule cls name m r

-- | Class for things that can be expected.  This includes 'Rule's, but also
-- bare 'Matcher's and 'Action's with no explicit response.
class Expectable cls name m r ex | ex -> cls name m r where
  -- | Converts an expectable to a Rule that means the same thing.
  toRule :: ex -> Rule cls name m r

instance Expectable cls name m r (Rule cls name m r) where
  toRule = id

instance Expectable cls name m r (Matcher (cls m) name r) where
  toRule m = JustMatcher m :=> []

instance Expectable cls name m r (WholeMethodMatcher (cls m) name r) where
  toRule m = m :=> []

-- | An 'Action' stands in for the 'Matcher' that matches it exactly.
instance
  ExactMatch (cls m) name r =>
  Expectable cls name m r (Action (cls m) name r)
  where
  toRule a = JustMatcher (exactMatcher a) :=> []

-- | Gets a 'Rule' ready to install: runs the class's setup if it hasn't run
-- yet, and closes each response over the current environment so that the
-- engine can run it in the base monad.
prepareRule ::
  forall cls name m r.
  (MockableMethod cls name m r, MonadIO m) =>
  Rule cls name m r ->
  MockSetup m (Internal.Rule (cls m) name m r)
prepareRule (m :=> rs) = do
  initClassIfNeeded (Proxy :: Proxy cls)
  env <- askMockEnv
  return (m Internal.:=> map (\f action -> runMockTWith (f action) env) rs)
