{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains MockT and SetupMockT state functions.
module Test.HMock.Internal.State where

import Control.Monad (forM_, unless, (<=<))
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Extra (maybeM)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Reader (MonadReader (..), ReaderT, mapReaderT, runReaderT)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer (MonadWriter)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.Stack (withFrozenCallStack)
import Test.HMock.ExpectContext (ExpectContext (..))
import Test.HMock.Internal.ExpectSet (ExpectSet (..), getSteps)
import Test.HMock.Internal.Step (SingleRule, Step (..), unwrapExpected)
import Test.HMock.Internal.Util (Located)
import Test.HMock.Mockable (Mockable (..))
import UnliftIO
  ( MonadIO,
    MonadUnliftIO,
    STM,
    TVar,
    atomically,
    modifyTVar,
    newTVarIO,
    readTVar,
    readTVarIO,
  )

-- | Full state of a mock.
data MockState m = MockState
  { mockExpectSet :: TVar (ExpectSet (Step m)),
    mockDefaults :: TVar [(Bool, Step m)],
    mockCheckAmbiguity :: TVar Bool,
    mockClasses :: TVar (Set TypeRep),
    mockParent :: Maybe (MockState m)
  }

-- | Initializes a new 'MockState' with the given parent.  If the parent is
-- 'Nothing', then a new root state is made.
initMockState :: MonadIO m => Maybe (MockState m) -> m (MockState m)
initMockState parent =
  MockState
    <$> newTVarIO ExpectNothing
    <*> newTVarIO []
    <*> maybeM
      (newTVarIO False)
      (newTVarIO <=< readTVarIO . mockCheckAmbiguity)
      (return parent)
    <*> maybe (newTVarIO Set.empty) (return . mockClasses) parent
    <*> pure parent

-- | Gets a list of all states, starting with the innermost.
allStates :: MockState m -> [MockState m]
allStates s
  | Just s' <- mockParent s = s : allStates s'
  | otherwise = [s]

-- | Gets the root state.
rootState :: MockState m -> MockState m
rootState = last . allStates

-- | Monad for setting up a mockable class.  Note that even though the type
-- looks that way, this is *not* a monad transformer.  It's a very restricted
-- environment that can only be used to set up defaults for a class.
newtype MockSetup m a where
  MockSetup :: {unMockSetup :: ReaderT (MockState m) STM a} -> MockSetup m a
  deriving (Functor, Applicative, Monad)

-- | Runs a setup action with the root state, rather than the current one. 
runInRootState :: MockSetup m a -> MockSetup m a
runInRootState = MockSetup . local rootState . unMockSetup

-- | Run an STM action in 'MockSetup'
mockSetupSTM :: STM a -> MockSetup m a
mockSetupSTM m = MockSetup (lift m)

-- | Runs class initialization for a 'Mockable' class, if it hasn't been run
-- yet.
initClassIfNeeded ::
  forall cls m proxy.
  (Mockable cls, Typeable m, MonadIO m) =>
  proxy cls ->
  MockSetup m ()
initClassIfNeeded proxy = runInRootState $ do
  state <- MockSetup ask
  classes <- mockSetupSTM $ readTVar (mockClasses state)
  unless (Set.member t classes) $ do
    mockSetupSTM $ modifyTVar (mockClasses state) (Set.insert t)
    setupMockable (Proxy :: Proxy cls)
  where
    t = typeRep proxy

-- | Runs class initialization for all uninitialized 'Mockable' classes in the
-- given 'ExpectSet'.
initClassesAsNeeded :: MonadIO m => ExpectSet (Step m) -> MockSetup m ()
initClassesAsNeeded es = runInRootState $
  forM_ (getSteps es) $
    \(Step (_ :: Located (SingleRule cls name m r))) ->
      initClassIfNeeded (Proxy :: Proxy cls)

-- | Monad transformer for running mocks.
newtype MockT m a where
  MockT :: {unMockT :: ReaderT (MockState m) m a} -> MockT m a
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

-- | Applies a function to the base monad of 'MockT'.
mapMockT :: (m a -> m b) -> MockT m a -> MockT m b
mapMockT f = MockT . mapReaderT f . unMockT

instance MonadReader r m => MonadReader r (MockT m) where
  ask = lift ask
  local = mapMockT . local
  reader = lift . reader

-- | This type class defines a shared API between the 'MockT' and 'MockSetup'
-- monads.
class MockContext ctx where
  -- | Runs a 'MockSetup' action in this monad.
  fromMockSetup :: MonadIO m => MockSetup m a -> ctx m a

instance MockContext MockSetup where
  fromMockSetup = id

instance MockContext MockT where
  fromMockSetup m = do
    state <- MockT ask
    atomically $ runReaderT (unMockSetup m) state

-- | Adds an expectation to the 'MockState' for the given 'ExpectSet',
-- interleaved with any existing expectations.
expectThisSet :: MonadIO m => ExpectSet (Step m) -> MockT m ()
expectThisSet e = fromMockSetup $ do
  initClassesAsNeeded e
  state <- MockSetup ask
  mockSetupSTM $ modifyTVar (mockExpectSet state) (e `ExpectInterleave`)

instance ExpectContext MockT where
  expect e =
    withFrozenCallStack $ expectThisSet $ unwrapExpected $ expect e
  expectN mult e =
    withFrozenCallStack $ expectThisSet $ unwrapExpected $ expectN mult e
  expectAny e =
    withFrozenCallStack $ expectThisSet $ unwrapExpected $ expectAny e
  inSequence es =
    withFrozenCallStack $ expectThisSet $ unwrapExpected $ inSequence es
  inAnyOrder es =
    withFrozenCallStack $ expectThisSet $ unwrapExpected $ inAnyOrder es
  anyOf es =
    withFrozenCallStack $ expectThisSet $ unwrapExpected $ anyOf es
  times mult es =
    withFrozenCallStack $ expectThisSet $ unwrapExpected $ times mult es
  consecutiveTimes mult es =
    withFrozenCallStack $
      expectThisSet $ unwrapExpected $ consecutiveTimes mult es
