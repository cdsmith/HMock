{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines monads for working with mocks.  HMock tests run in the
-- t'MockT' monad transformer.  A more limited monad, t'MockSetup', is used for
-- setting up defaults for each class.  Both are instances of the 'MockContext'
-- class, which defines a shared API.
module Test.HMock.Classic.MockT
  ( MockT,
    runMockT,
    withMockT,
    nestMockT,
    withNestedMockT,
    Severity (..),
    setAmbiguityCheck,
    setUninterestingActionCheck,
    setUnexpectedActionCheck,
    setUnmetExpectationCheck,
    describeExpectations,
    verifyExpectations,
    Mockable (..),
    MockSetup,
    MockContext,
  )
where

import Control.Monad (join)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans (lift)
import qualified Data.Set as Set
import Test.HMock.Classic.Internal
import Test.HMock.Internal.ExpectSet (ExpectSet (ExpectNothing), excess, formatExpectSet)
import Test.HMock.Internal.State
  ( MockState (..),
    Severity (..),
    askState,
    describeExpectationsWith,
    initMockState,
    liftSTM,
    reportFault,
  )
import UnliftIO (MonadIO, TVar, newTVarIO, readTVar, writeTVar)

-- | Runs a test in the t'MockT' monad, handling all of the mocks.
runMockT :: forall m a. MonadIO m => MockT m a -> m a
runMockT test = withMockT constTest
  where
    constTest :: (forall b. MockT m b -> m b) -> MockT m a
    constTest _inMockT = test

-- | Runs a test in the t'MockT' monad.  The test can unlift other MockT pieces
-- to the base monad while still acting on the same set of expectations.  This
-- can be useful for testing concurrency or similar mechanisms.
--
-- @
-- test = 'withMockT' '$' \inMockT -> do
--    'Test.HMock.Classic.ExpectContext.expect' '$' ...
--
--    'Control.Monad.IO.Class.liftIO' '$' 'Control.Concurrent.forkIO' '$' inMockT firstThread
--    'Control.Monad.IO.Class.liftIO' '$' 'Control.Concurrent.forkIO' '$' inMockT secondThread
-- @
--
-- This is a low-level primitive.  Consider using the @unliftio@ package for
-- higher level implementations of multithreading and other primitives.
withMockT ::
  forall m b. MonadIO m => ((forall a. MockT m a -> m a) -> MockT m b) -> m b
withMockT test = do
  state <- initMockState Nothing
  classes <- newTVarIO Set.empty
  let env = MockEnv state classes
      inMockT :: forall a. MockT m a -> m a
      inMockT m = runMockTWith m env
  flip runMockTWith env $ do
    a <- test inMockT
    verifyExpectations
    return a

-- | Starts a nested block within t'MockT'.  The nested block has its own set of
-- expectations, which must be fulfilled before the end of the block.  Outer
-- expectations still apply inside it, and their responses still run in the
-- outer block.
--
-- Beware: use of 'nestMockT' might signify that you are doing too much in a
-- single test.  Consider splitting large tests into a separate test for each
-- case.
nestMockT :: forall m a. MonadIO m => MockT m a -> MockT m a
nestMockT nest = withNestedMockT constNest
  where
    constNest :: (forall b. MockT m b -> m b) -> MockT m a
    constNest _inMockT = nest

-- | Starts a nested block within t'MockT'.  The nested block has its own set of
-- expectations, which must be fulfilled before the end of the block.  Outer
-- expectations still apply inside it, and their responses still run in the
-- outer block.  It can unlift other MockT pieces to the base monad while
-- still acting on the same set of expectations.  This can be useful for
-- testing concurrency or similar mechanisms.
--
-- Beware: use of 'withNestedMockT' might signify that you are doing too much
-- in a single test.  Consider splitting large tests into a separate test for
-- each case.
withNestedMockT ::
  forall m b.
  MonadIO m =>
  ((forall a. MockT m a -> m a) -> MockT m b) ->
  MockT m b
withNestedMockT nest = do
  parent <- MockT ask
  state <- lift $ initMockState (Just (mockEnvState parent))
  let env = parent {mockEnvState = state}
  withEnv env $ do
    a <- nest (`runMockTWith` env)
    verifyExpectations
    return a
  where
    withEnv env = MockT . local (const env) . unMockT

setSeverity :: MonadIO m => (MockState m -> TVar Severity) -> Severity -> MockT m ()
setSeverity field severity = fromMockSetup $ liftMockSTM $ do
  state <- askState
  liftSTM $ writeTVar (field state) severity

-- | Sets the severity for ambiguous actions.  An ambiguous action is one that
-- matches expectations in more than one way.  If this is not set to `Error`,
-- the most recently added expectation will take precedence.
--
-- This defaults to 'Ignore'.
setAmbiguityCheck :: MonadIO m => Severity -> MockT m ()
setAmbiguityCheck = setSeverity mockAmbiguitySeverity

-- | Sets the severity for uninteresting actions.  An uninteresting action is
-- one for which no expectations or other configuration have been added that
-- mention the method at all.  If this is not set to `Error`, then uninteresting
-- methods are treated just like unexpected methods.
--
-- Before you weaken this check, consider that the labeling of methods as
-- "uninteresting" is non-compositional.  A change in one part of your test can
-- result in a formerly uninteresting action being considered interesting in a
-- different part of the test.
--
-- This defaults to 'Error'.
setUninterestingActionCheck :: MonadIO m => Severity -> MockT m ()
setUninterestingActionCheck = setSeverity mockUninterestingSeverity

-- | Sets the severity for unexpected actions.  An unexpected action is one that
-- doesn't match any expectations *and* isn't explicitly allowed by
-- 'Test.HMock.Classic.Defaults.allowUnexpected'.  If this is not set to
-- `Error`, the action returns its default response.
--
-- This defaults to 'Error'.
setUnexpectedActionCheck :: MonadIO m => Severity -> MockT m ()
setUnexpectedActionCheck = setSeverity mockUnexpectedSeverity

-- | Sets the severity for unmet expectations.  An unmet expectation happens
-- when an expectation is added, but either the test (or nesting level) ends or
-- 'verifyExpectations' is used before a matching action takes place.
--
-- This defaults to 'Error'.
setUnmetExpectationCheck :: MonadIO m => Severity -> MockT m ()
setUnmetExpectationCheck = setSeverity mockUnmetSeverity

-- | Fetches a 'String' that describes the current set of outstanding
-- expectations.  This is sometimes useful for debugging test code.  The exact
-- format is not specified.
describeExpectations :: MonadIO m => MockT m String
describeExpectations = withMockEnv (describeExpectationsWith . mockEnvState)

-- | Verifies that all mock expectations are satisfied.  If there is a nested
-- block in effect, only the expectations of that nested block are verified
-- You normally don't need to do this, because it happens automatically at the
-- end of your test or nested block.  However, it's occasionally useful to check
-- expectations early.
--
-- Beware: use of 'verifyExpectations' might signify that you are doing too much
-- in a single test.  Consider splitting large tests into a separate test for
-- each case.
verifyExpectations :: MonadIO m => MockT m ()
verifyExpectations = join $
  fromMockSetup $ liftMockSTM $ do
    state <- askState
    expectSet <- liftSTM $ readTVar $ mockExpectSet state
    missingSev <- liftSTM $ readTVar $ mockUnmetSeverity state
    case excess expectSet of
      ExpectNothing -> return (return ())
      missing ->
        return $
          reportFault missingSev $
            "Unmet expectations:\n" ++ formatExpectSet missing
