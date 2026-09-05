{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

-- | The mutable state behind a mock, and the t'MockSTM' monad that reads and
-- writes it transactionally.  Shared by every front end; nothing here knows
-- how a test is run.
module Test.HMock.Internal.State where

import Control.Monad (forM_, (<=<))
import Control.Monad.Extra (maybeM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), runReaderT)
import Control.Monad.Trans (lift)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownSymbol, symbolVal)
import System.IO (hPutStrLn, stderr)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Test.HMock.Internal.ExpectSet (ExpectSet (..), formatExpectSet, getSteps)
import Test.HMock.Internal.Step (SingleRule, Step (..))
import Test.HMock.Internal.Util (InstanceId (..), Located)
import UnliftIO
  ( MonadIO,
    STM,
    TVar,
    atomically,
    modifyTVar,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
  )


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
    mockInterestingMethods :: TVar (Set (TypeRep, String)),
    mockNextInstanceId :: TVar Int,
    mockInstanceNames :: TVar (Set String),
    mockParent :: Maybe (MockState m)
  }

-- | Initializes a new t'MockState' with the given parent.  If the parent is
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
    <*> maybe (newTVarIO Set.empty) (return . mockInterestingMethods) parent
    <*> maybe (newTVarIO 1) (return . mockNextInstanceId) parent
    <*> maybe (newTVarIO Set.empty) (return . mockInstanceNames) parent
    <*> pure parent

-- | Allocates an identity for a new mocked instance, with the given name if
-- there is one.  Numbers and names come from the root state, so both are
-- unique across a whole test even when nested blocks are in play.  An
-- unnamed instance is named for its number, as @"#3"@.  A name already in
-- use, whether given or not, gets a numeric suffix: @"fs"@, then @"fs_2"@,
-- @"fs_3"@, and so on.
newInstanceId :: Maybe String -> MockSTM m InstanceId
newInstanceId requested = runInRootState $ do
  state <- askState
  n <- liftSTM $ readTVar (mockNextInstanceId state)
  liftSTM $ writeTVar (mockNextInstanceId state) (n + 1)
  used <- liftSTM $ readTVar (mockInstanceNames state)
  let base = fromMaybe ('#' : show n) requested
      candidate :: Int -> String
      candidate 1 = base
      candidate k = base ++ "_" ++ show k
      name = candidate (until ((`Set.notMember` used) . candidate) (+ 1) 1)
  liftSTM $ modifyTVar (mockInstanceNames state) (Set.insert name)
  return (InstanceId n name)

-- | Gets a list of all states, starting with the innermost.
allStates :: MockState m -> [MockState m]
allStates s
  | Just s' <- mockParent s = s : allStates s'
  | otherwise = [s]

-- | Gets the root state.
rootState :: MockState m -> MockState m
rootState = last . allStates

-- | A transaction on a t'MockState'.  Every read or write of the state goes
-- through one of these, so that a match, or the installation of an
-- expectation, is atomic.  The @m@ parameter is only there because the state
-- stores responses in @m@; this is not a transformer over @m@.
newtype MockSTM m a where
  MockSTM :: {unMockSTM :: ReaderT (MockState m) STM a} -> MockSTM m a
  deriving (Functor, Applicative, Monad)

-- | Runs a transaction against a given state.
runMockSTM :: MockState m -> MockSTM m a -> STM a
runMockSTM state = flip runReaderT state . unMockSTM

-- | The state the transaction is running against.
askState :: MockSTM m (MockState m)
askState = MockSTM ask

-- | Runs a transaction with the root state, rather than the current one.
runInRootState :: MockSTM m a -> MockSTM m a
runInRootState = MockSTM . local rootState . unMockSTM

-- | Lifts a raw STM action into t'MockSTM'.
liftSTM :: STM a -> MockSTM m a
liftSTM m = MockSTM (lift m)

-- | Marks a method as "interesting".  This can have implications for what
-- happens to calls to that method.
markInteresting ::
  forall k (cls :: k) name m proxy1 proxy2.
  (Typeable cls, KnownSymbol name) =>
  proxy1 cls ->
  proxy2 name ->
  MockSTM m ()
markInteresting proxyCls proxyName = runInRootState $ do
  state <- askState
  liftSTM $
    modifyTVar
      (mockInterestingMethods state)
      (Set.insert (typeRep proxyCls, symbolVal proxyName))

-- | Determines whether a method is "interesting".
isInteresting :: 
  forall k (cls :: k) name m proxy1 proxy2.
  (Typeable cls, KnownSymbol name) =>
  proxy1 cls ->
  proxy2 name ->
  MockSTM m Bool
isInteresting proxyCls proxyName = runInRootState $ do
  state <- askState
  interesting <- liftSTM $ readTVar (mockInterestingMethods state)
  return ((typeRep proxyCls, symbolVal proxyName) `Set.member` interesting)

-- | Marks every method mentioned in the given 'ExpectSet' as interesting.
markInterestingSteps :: ExpectSet (Step m) -> MockSTM m ()
markInterestingSteps es =
  forM_ (getSteps es) $
    \(Step _ (_ :: Located (SingleRule cls name m r))) ->
      markInteresting (Proxy :: Proxy cls) (Proxy :: Proxy name)

-- | Adds an expectation to the t'MockState' for the given 'ExpectSet',
-- interleaved with any existing expectations.
expectThisSet :: ExpectSet (Step m) -> MockSTM m ()
expectThisSet e = do
  markInterestingSteps e
  state <- askState
  liftSTM $ modifyTVar (mockExpectSet state) (e `ExpectInterleave`)

-- | Reports a potential problem with the given 'Severity'.
reportFault :: (HasCallStack, MonadIO m) => Severity -> String -> m ()
reportFault severity msg = case severity of
  Ignore -> return ()
  Warning -> liftIO $ hPutStrLn stderr msg
  Error -> error msg

-- | Describes the outstanding expectations in the given state.  The exact
-- format is not specified.
describeExpectationsWith :: MonadIO m => MockState m -> m String
describeExpectationsWith state = atomically $ runMockSTM state $ do
  states <- allStates <$> askState
  expectSets <- mapM (liftSTM . readTVar . mockExpectSet) states
  return $
    intercalate "\n----- (next layer) -----\n" $
      formatExpectSet <$> expectSets
