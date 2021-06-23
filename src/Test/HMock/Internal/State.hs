{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains MockT and SetupMockT state functions.
module Test.HMock.Internal.State where

import Control.Monad (forM_, unless)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
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
  )

#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Fail (MonadFail)
#endif

-- | Full state of a mock.
data MockState m = MockState
  { mockExpectSet :: TVar (ExpectSet (Step m)),
    mockDefaults :: TVar [(Bool, Step m)],
    mockClasses :: TVar (Set TypeRep),
    mockCheckAmbiguity :: TVar Bool
  }

-- | Initializes a new 'MockState' with a blank slate.
initMockState :: MonadIO m => m (MockState m)
initMockState =
  MockState
    <$> newTVarIO ExpectNothing
    <*> newTVarIO []
    <*> newTVarIO Set.empty
    <*> newTVarIO False

-- | Monad for setting up a mockable class.  Note that even though the type
-- looks that way, this is *not* a monad transformer.  It's a very restricted
-- environment that can only be used to set up defaults for the class.
newtype MockSetupT m a where
  MockSetupT :: {unMockSetupT :: ReaderT (MockState m) STM a} -> MockSetupT m a
  deriving (Functor, Applicative, Monad)

-- | Run an STM action in 'MockSetupT'
mockSetupSTM :: STM a -> MockSetupT m a
mockSetupSTM m = MockSetupT (lift m)

-- | Runs class initialization for a 'Mockable' class, if it hasn't been run
-- yet.
initClassIfNeeded ::
  forall cls m proxy.
  (Mockable cls, Typeable m, MonadIO m) =>
  proxy cls ->
  MockSetupT m ()
initClassIfNeeded proxy = do
  state <- MockSetupT ask
  classes <- mockSetupSTM $ readTVar (mockClasses state)
  unless (Set.member t classes) $ do
    mockSetupSTM $ modifyTVar (mockClasses state) (Set.insert t)
    setupMockable (Proxy :: Proxy cls)
  where
    t = typeRep proxy

-- | Runs class initialization for all uninitialized 'Mockable' classes in the
-- given 'ExpectSet'.
initClassesAsNeeded :: MonadIO m => ExpectSet (Step m) -> MockSetupT m ()
initClassesAsNeeded es = forM_ (getSteps es) $
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

-- | Type class for contexts in which defaults can be set up for a mockable
-- class.  Notably, this includes `MockSetupT` and `MockT`.
class MockSetupContext ctx where
  -- | Runs a 'MockSetupT' action in this monad.
  fromMockSetup :: MonadIO m => MockSetupT m a -> ctx m a

instance MockSetupContext MockSetupT where
  fromMockSetup = id

instance MockSetupContext MockT where
  fromMockSetup m = do
    state <- MockT ask
    atomically $ runReaderT (unMockSetupT m) state

-- | Adds an expectation to the 'MockState' for the given 'ExpectSet',
-- interleaved with any existing expectations.
expectThisSet :: MonadIO m => ExpectSet (Step m) -> MockT m ()
expectThisSet e = fromMockSetup $ do
  initClassesAsNeeded e
  state <- MockSetupT ask
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
