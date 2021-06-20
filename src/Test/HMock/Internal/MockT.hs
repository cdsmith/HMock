{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.HMock.Internal.MockT where

import Control.Concurrent (ThreadId)
import Control.Monad (forM_, join, unless, when)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Reader
  ( MonadReader (ask, local, reader),
    ReaderT,
    mapReaderT,
    runReaderT,
  )
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer (MonadWriter)
import Data.Default (Default (def))
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (TypeRep, cast, typeRep)
import GHC.Stack
import Test.HMock.Internal.ExpectSet
import Test.HMock.Internal.Expectable
import Test.HMock.Internal.Mockable
import Test.HMock.Internal.Util (Located (..), locate, withLoc)
import UnliftIO
import UnliftIO.Concurrent (myThreadId)

#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Fail (MonadFail)
#endif

data MockState m = MockState
  { mockExpectSet :: TVar (ExpectSet (Step m)),
    mockDefaults :: TVar [Step m],
    mockClasses :: TVar (Set TypeRep),
    mockInitingClasses :: TVar (Map TypeRep ThreadId)
  }

initMockState :: MonadIO m => m (MockState m)
initMockState =
  MockState
    <$> newTVarIO ExpectNothing
    <*> newTVarIO []
    <*> newTVarIO Set.empty
    <*> newTVarIO Map.empty

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

mapMockT :: (m a -> m b) -> MockT m a -> MockT m b
mapMockT f = MockT . mapReaderT f . unMockT

initClassIfNeeded ::
  forall cls m proxy.
  (Mockable cls, Typeable m, MonadIO m) =>
  proxy cls ->
  MockT m ()
initClassIfNeeded proxy =
  do
    state <- MockT ask
    myTid <- myThreadId
    needsInit <- atomically $ do
      finishedClasses <- readTVar (mockClasses state)
      if Set.member t finishedClasses
        then return False
        else do
          initingClasses <- readTVar (mockInitingClasses state)
          case Map.lookup t initingClasses of
            Nothing -> do
              writeTVar
                (mockInitingClasses state)
                (Map.insert t myTid initingClasses)
              return True
            Just tid
              | tid == myTid -> return False
              | otherwise -> retrySTM
    when needsInit $ do
      setupMockable (Proxy :: Proxy cls)
      atomically $ do
        modifyTVar (mockClasses state) (Set.insert t)
        modifyTVar (mockInitingClasses state) (Map.delete t)
  where
    t = typeRep proxy

initClassesAsNeeded :: MonadIO m => ExpectSet (Step m) -> MockT m ()
initClassesAsNeeded es = forM_ (getSteps es) $
  \(Step (_ :: Located (SingleRule cls name m r))) ->
    initClassIfNeeded (Proxy :: Proxy cls)

instance MonadReader r m => MonadReader r (MockT m) where
  ask = lift ask
  local = mapMockT . local
  reader = lift . reader

instance ExpectContext MockT where
  fromExpectSet e = do
    initClassesAsNeeded e
    state <- MockT ask
    atomically $ modifyTVar (mockExpectSet state) (e `ExpectInterleave`)

-- | Runs a test in the 'MockT' monad, handling all of the mocks.
runMockT :: forall m a. MonadIO m => MockT m a -> m a
runMockT test = withMockT constTest
  where
    constTest :: (forall b. MockT m b -> m b) -> MockT m a
    constTest _inMockT = test

-- | Runs a test in the 'MockT' monad.  The test can unlift other MockT pieces
-- to the base monad while still acting on the same set of expectations.  This
-- can be useful for testing concurrency or similar mechanisms.
--
-- @
-- test = 'withMockT' '$' \inMockT -> do
--    'expect' '$' ...
--
--    'liftIO' '$' 'forkIO' '$' inMockT firstThread
--    'liftIO' '$' 'forkIO' '$' inMockT secondThread
-- @
--
-- This is a low-level primitive.  Consider using the @unliftio@ package for
-- higher level implementations of multithreading and other primitives.
withMockT ::
  forall m b. MonadIO m => ((forall a. MockT m a -> m a) -> MockT m b) -> m b
withMockT test = do
  state <- initMockState
  let inMockT :: forall a. MockT m a -> m a
      inMockT m = runReaderT (unMockT m) state
  flip runReaderT state $
    unMockT $ do
      a <- test inMockT
      verifyExpectations
      return a

-- | Fetches a 'String' that describes the current set of outstanding
-- expectations.  This is sometimes useful for debugging test code.  The exact
-- format is not specified.
describeExpectations :: MonadIO m => MockT m String
describeExpectations =
  formatExpectSet <$> (MockT ask >>= readTVarIO . mockExpectSet)

-- | Verifies that all mock expectations are satisfied.  You normally don't need
-- to do this, because it happens automatically at the end of your test in
-- 'runMockT'.  However, it's occasionally useful to check expectations in the
-- middle of a test, such as before going on to the next stage.
--
-- Use of 'verifyExpectations' might signify that you are doing too much in a
-- single test.  Consider splitting large tests into a separate test for each
-- case.
verifyExpectations :: MonadIO m => MockT m ()
verifyExpectations = do
  expectSet <- MockT ask >>= readTVarIO . mockExpectSet
  unless (satisfied expectSet) $ do
    case excess expectSet of
      ExpectNothing -> return ()
      missing -> error $ "Unmet expectations:\n" ++ formatExpectSet missing

-- | Changes the default response for matching actions.
--
-- Without 'byDefault', actions with no explicit response will return the
-- 'Default' value for the type, or 'undefined' if the return type isn't an
-- instance of 'Default'.  'byDefault' replaces that with a new default
-- response, also overriding any previous defaults. The rule passed in must have
-- exactly one response.
byDefault ::
  forall cls name m r.
  (MonadIO m, MockableMethod cls name m r) =>
  Rule cls name m r ->
  MockT m ()
byDefault (m :=> [r]) = do
  initClassIfNeeded (Proxy :: Proxy cls)
  state <- MockT ask
  atomically $
    modifyTVar' (mockDefaults state) (Step (locate callStack (m :-> Just r)) :)
byDefault _ = error "Defaults must have exactly one response."

mockMethodImpl ::
  forall cls name m r.
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r
  ) =>
  r ->
  Action cls name m r ->
  MockT m r
mockMethodImpl surrogate action =
  do
    initClassIfNeeded (Proxy :: Proxy cls)
    state <- MockT ask
    join $
      atomically $ do
        expectSet <- readTVar (mockExpectSet state)
        defaults <- readTVar (mockDefaults state)
        let (newExpectSet, response) = decideAction expectSet defaults
        writeTVar (mockExpectSet state) newExpectSet
        return response
  where
    decideAction expectSet defaults =
      let (partial, full) = partitionEithers (tryMatch <$> liveSteps expectSet)
          orderedPartial = snd <$> sortBy (compare `on` fst) (catMaybes partial)
       in case (full, findDefault defaults, surrogate, orderedPartial) of
            ((e, Just response) : _, _, _, _) -> (e, response)
            ((e, Nothing) : _, d, s, _) -> (e, fromMaybe (return s) d)
            ([], _, _, []) -> error $ noMatchError action
            ([], _, _, _) -> error $ partialMatchError action orderedPartial
    tryMatch ::
      (Step m, ExpectSet (Step m)) ->
      Either (Maybe (Int, String)) (ExpectSet (Step m), Maybe (MockT m r))
    tryMatch (Step expected, e)
      | Just lrule@(Loc _ (m :-> impl)) <- cast expected =
        case matchAction m action of
          NoMatch n ->
            Left (Just (n, withLoc (showMatcher (Just action) m <$ lrule)))
          Match ->
            Right (e, ($ action) <$> impl)
      | otherwise = Left Nothing

    findDefault :: [Step m] -> Maybe (MockT m r)
    findDefault [] = Nothing
    findDefault (Step expected : _)
      | Just (Loc _ (m :-> Just r)) <- cast expected,
        Match <- matchAction m action =
        Just (r action)
    findDefault (_ : steps) = findDefault steps

-- | Implements a method in a 'Mockable' monad by delegating to the mock
-- framework.  If the method is called unexpectedly, an exception will be
-- thrown.  However, an expected invocation without a specified response will
-- return the default value.
mockMethod ::
  forall cls name m r.
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r,
    Default r
  ) =>
  Action cls name m r ->
  MockT m r
mockMethod action =
  withFrozenCallStack $ mockMethodImpl def action

-- | Implements a method in a 'Mockable' monad by delegating to the mock
-- framework.  If the method is called unexpectedly, an exception will be
-- thrown.  However, an expected invocation without a specified response will
-- return undefined.  This can be used in place of 'mockMethod' when the return
-- type has no default.
mockDefaultlessMethod ::
  forall cls name m r.
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r
  ) =>
  Action cls name m r ->
  MockT m r
mockDefaultlessMethod action =
  withFrozenCallStack $ mockMethodImpl undefined action

-- An error for an action that matches no expectations at all.
noMatchError ::
  (HasCallStack, Mockable cls) =>
  -- | The action that was received.
  Action cls name m a ->
  String
noMatchError a =
  "Unexpected action: " ++ showAction a

-- An error for an action that doesn't match the argument predicates for any
-- of the method's expectations.
partialMatchError ::
  (HasCallStack, Mockable cls) =>
  -- | The action that was received.
  Action cls name m a ->
  -- | Descriptions of the matchers that most closely matched, closest first.
  [String] ->
  String
partialMatchError a partials =
  "Wrong arguments: "
    ++ showAction a
    ++ "\n\nClosest matches:\n - "
    ++ intercalate "\n - " (take 5 partials)
