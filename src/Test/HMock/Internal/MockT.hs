{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the 'MockT' monad transformer.
module Test.HMock.Internal.MockT
  ( MockT,
    runMockT,
    withMockT,
    describeExpectations,
    verifyExpectations,
    setAmbiguityCheck,
  )
where

import Control.Monad (unless)
import Control.Monad.Reader
  ( MonadReader (..),
    runReaderT,
  )
import Test.HMock.Internal.ExpectSet
import Test.HMock.Internal.State
import UnliftIO

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
--    'Test.HMock.Expectable.expect' '$' ...
--
--    'liftIO' '$' 'Control.Concurrent.forkIO' '$' inMockT firstThread
--    'liftIO' '$' 'Control.Concurrent.forkIO' '$' inMockT secondThread
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

-- | Sets whether to check for ambiguous actions.  If 'True', then actions that
-- match expectations in more than one way will fail.  If 'False', then the
-- most recently added action will take precedence.  This defaults to 'False'.
setAmbiguityCheck :: MonadIO m => Bool -> MockT m ()
setAmbiguityCheck flag =
  MockT ask >>= atomically . flip writeTVar flag . mockCheckAmbiguity
