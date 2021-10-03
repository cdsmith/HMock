{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}

-- | This module contains MockT and SetupMockT state functions.
module Test.HMock.Internal.State where

import Control.Monad (forM_, unless, (<=<))
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Extra (maybeM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Reader (MonadReader (..), ReaderT, mapReaderT, runReaderT)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer (MonadWriter)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.Stack (withFrozenCallStack, HasCallStack)
import GHC.TypeLits (KnownSymbol, symbolVal)
import System.IO (hPutStrLn, stderr)
import Test.HMock.ExpectContext (ExpectContext (..))
import Test.HMock.Internal.ExpectSet (ExpectSet (..), getSteps)
import Test.HMock.Internal.Step (SingleRule, Step (..), unwrapExpected)
import Test.HMock.Internal.Util (Located)
import Test.HMock.Mockable (Mockable (..))
import UnliftIO
  ( MonadIO,
    MonadUnliftIO(withRunInIO),
    STM,
    TVar,
    atomically,
    modifyTVar,
    newTVarIO,
    readTVar,
    readTVarIO,
  )
import Data.Kind (Type, Constraint)

#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Fail (MonadFail)
#endif

-- | The severity for a possible problem.
data Severity
  = -- | Fail the test.
    Error
  | -- | Print a message, but continue the test.
    Warning
  | -- | Don't do anything.
    Ignore

-- | Full state of a mock.
data MockState m = MockState
  { mockExpectSet :: TVar (ExpectSet (Step m)),
    mockDefaults :: TVar [Step m],
    mockAllowUnexpected :: TVar [Step m],
    mockSideEffects :: TVar [Step m],
    mockAmbiguitySeverity :: TVar Severity,
    mockUnexpectedSeverity :: TVar Severity,
    mockUninterestingSeverity :: TVar Severity,
    mockUnmetSeverity :: TVar Severity,
    mockClasses :: TVar (Set TypeRep),
    mockInterestingMethods :: TVar (Set (TypeRep, String)),
    mockParent :: Maybe (MockState m)
  }

-- | Initializes a new 'MockState' with the given parent.  If the parent is
-- 'Nothing', then a new root state is made.
initMockState :: MonadIO m => Maybe (MockState m) -> m (MockState m)
initMockState parent =
  MockState
    <$> newTVarIO ExpectNothing
    <*> newTVarIO []
    <*> newTVarIO []
    <*> newTVarIO []
    <*> maybeM
      (newTVarIO Ignore)
      (newTVarIO <=< readTVarIO . mockAmbiguitySeverity)
      (return parent)
    <*> maybeM
      (newTVarIO Error)
      (newTVarIO <=< readTVarIO . mockUnexpectedSeverity)
      (return parent)
    <*> maybeM
      (newTVarIO Error)
      (newTVarIO <=< readTVarIO . mockUninterestingSeverity)
      (return parent)
    <*> maybeM
      (newTVarIO Error)
      (newTVarIO <=< readTVarIO . mockUnmetSeverity)
      (return parent)
    <*> maybe (newTVarIO Set.empty) (return . mockClasses) parent
    <*> maybe (newTVarIO Set.empty) (return . mockInterestingMethods) parent
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

-- | Marks a method as "interesting".  This can have implications for what
-- happens to calls to that method.
markInteresting ::
  forall (cls :: (Type -> Type) -> Constraint) name m proxy1 proxy2.
  (Typeable cls, KnownSymbol name) =>
  proxy1 cls ->
  proxy2 name ->
  MockSetup m ()
markInteresting proxyCls proxyName = runInRootState $ do
  state <- MockSetup ask
  mockSetupSTM $
    modifyTVar
      (mockInterestingMethods state)
      (Set.insert (typeRep proxyCls, symbolVal proxyName))

-- | Determines whether a method is "interesting".
isInteresting :: 
  forall (cls :: (Type -> Type) -> Constraint) name m proxy1 proxy2.
  (Typeable cls, KnownSymbol name) =>
  proxy1 cls ->
  proxy2 name ->
  MockSetup m Bool
isInteresting proxyCls proxyName = runInRootState $ do
  state <- MockSetup ask
  interesting <- mockSetupSTM $ readTVar (mockInterestingMethods state)
  return ((typeRep proxyCls, symbolVal proxyName) `Set.member` interesting)

-- | Runs class initialization for all uninitialized 'Mockable' classes in the
-- given 'ExpectSet'.
initClassesAsNeeded :: MonadIO m => ExpectSet (Step m) -> MockSetup m ()
initClassesAsNeeded es = runInRootState $
  forM_ (getSteps es) $
    \(Step (_ :: Located (SingleRule cls name m r))) -> do
      initClassIfNeeded (Proxy :: Proxy cls)
      markInteresting (Proxy :: Proxy cls) (Proxy :: Proxy name)

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
      MonadThrow
    )

instance MonadTrans MockT where
  lift = MockT . lift

-- Note: The 'MonadUnliftIO' instance is implemented manually because deriving
-- it causes compilation failure in GHC 8.6 and 8.8.  (See issue #23.)
instance MonadUnliftIO m => MonadUnliftIO (MockT m) where
  withRunInIO inner = MockT $ withRunInIO $ \run -> inner (run . unMockT)

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
expectThisSet :: MonadIO m => ExpectSet (Step m) -> MockSetup m ()
expectThisSet e = do
  initClassesAsNeeded e
  state <- MockSetup ask
  mockSetupSTM $ modifyTVar (mockExpectSet state) (e `ExpectInterleave`)

-- | This instance allows you to add expectations from 'MockSetup' actions.
-- This is an unusual thing to do.  Consider using
-- 'Test.HMock.MockT.allowUnexpected', instead.
instance ExpectContext MockSetup where
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

instance ExpectContext MockT where
  expect e =
    withFrozenCallStack $
      fromMockSetup $ expectThisSet $ unwrapExpected $ expect e
  expectN mult e =
    withFrozenCallStack $
      fromMockSetup $ expectThisSet $ unwrapExpected $ expectN mult e
  expectAny e =
    withFrozenCallStack $
      fromMockSetup $ expectThisSet $ unwrapExpected $ expectAny e
  inSequence es =
    withFrozenCallStack $
      fromMockSetup $ expectThisSet $ unwrapExpected $ inSequence es
  inAnyOrder es =
    withFrozenCallStack $
      fromMockSetup $ expectThisSet $ unwrapExpected $ inAnyOrder es
  anyOf es =
    withFrozenCallStack $
      fromMockSetup $ expectThisSet $ unwrapExpected $ anyOf es
  times mult es =
    withFrozenCallStack $
      fromMockSetup $ expectThisSet $ unwrapExpected $ times mult es
  consecutiveTimes mult es =
    withFrozenCallStack $
      fromMockSetup $ expectThisSet $ unwrapExpected $ consecutiveTimes mult es

-- | Reports a potential problem with the given 'Severity'.
reportFault :: (HasCallStack, MonadIO m) => Severity -> String -> MockT m ()
reportFault severity msg = case severity of
  Ignore -> return ()
  Warning -> liftIO $ hPutStrLn stderr msg
  Error -> error msg
