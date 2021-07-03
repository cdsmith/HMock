{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines monads for working with mocks.  HMock tests run in the
-- 'MockT' monad transformer.  A more limited monad, 'MockSetup', is used for
-- setting up defaults for each class.  Both are instances of the 'MockContext'
-- monad, which defines a shared API.
module Test.HMock.MockT
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
    MockSetup,
    MockContext,
    allowUnexpected,
    byDefault,
    whenever,
  )
where

import Control.Monad (join)
import Control.Monad.Reader
  ( MonadReader (..),
    runReaderT,
  )
import Control.Monad.Trans (lift)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (Proxy))
import GHC.Stack (callStack)
import Test.HMock.ExpectContext (MockableMethod)
import Test.HMock.Internal.ExpectSet
import Test.HMock.Internal.Rule (Rule ((:=>)))
import Test.HMock.Internal.State
import Test.HMock.Internal.Step (SingleRule ((:->)), Step (Step))
import Test.HMock.Internal.Util (locate)
import Test.HMock.Rule (Expectable (toRule))
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
  state <- initMockState Nothing
  let inMockT :: forall a. MockT m a -> m a
      inMockT m = runReaderT (unMockT m) state
  flip runReaderT state $
    unMockT $ do
      a <- test inMockT
      verifyExpectations
      return a

-- | Starts a nested block within 'MockT'.  The nested block has its own set of
-- expectations, which must be fulfilled before the end of the block.
--
-- Beware: use of 'nestMockT' might signify that you are doing too much in a
-- single test.  Consider splitting large tests into a separate test for each
-- case.
nestMockT :: forall m a. MonadIO m => MockT m a -> MockT m a
nestMockT nest = withNestedMockT constNest
  where
    constNest :: (forall b. MockT m b -> m b) -> MockT m a
    constNest _inMockT = nest

-- | Starts a nested block within 'MockT'.  The nested block has its own set of
-- expectations, which must be fulfilled before the end of the block.  It can
-- unlift other MockT pieces to the base monad while still acting on the same
-- set of expectations.  This can be useful for testing concurrency or similar
-- mechanisms.
--
-- Beware: use of 'nestMockT' might signify that you are doing too much in a
-- single test.  Consider splitting large tests into a separate test for each
-- case.
withNestedMockT ::
  forall m b.
  MonadIO m =>
  ((forall a. MockT m a -> m a) -> MockT m b) ->
  MockT m b
withNestedMockT nest = do
  parent <- MockT ask
  state <- lift $ initMockState (Just parent)
  withState state $ do
    a <- nest (flip runReaderT state . unMockT)
    verifyExpectations
    return a
  where
    withState state = MockT . local (const state) . unMockT

-- | Sets the severity for ambiguous actions.  An ambiguous action is one that
-- matches expectations in more than one way.  If this is not set to `Error`,
-- the most recently added expectation will take precedence.
--
-- This defaults to 'Ignore'.
setAmbiguityCheck :: MonadIO m => Severity -> MockT m ()
setAmbiguityCheck severity = fromMockSetup $ do
  state <- MockSetup ask
  mockSetupSTM $ writeTVar (mockAmbiguitySeverity state) severity

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
setUninterestingActionCheck severity = fromMockSetup $ do
  state <- MockSetup ask
  mockSetupSTM $ writeTVar (mockUninterestingSeverity state) severity

-- | Sets the severity for unexpected actions.  An unexpected action is one that
-- doesn't match any expectations *and* isn't explicitly allowed by
-- `allowUnexpected`.  If this is not set to `Error`, the action returns its
-- default response.
--
-- This defaults to 'Error'.
setUnexpectedActionCheck :: MonadIO m => Severity -> MockT m ()
setUnexpectedActionCheck severity = fromMockSetup $ do
  state <- MockSetup ask
  mockSetupSTM $ writeTVar (mockUnexpectedSeverity state) severity

-- | Sets the severity for unmet expectations.  An unmet expectation happens
-- when an expectation is added, but either the test (or nesting level) ends or
-- 'verifyExpectations' is used before a matching action takes place.
--
-- This defaults to 'Error'.
setUnmetExpectationCheck :: MonadIO m => Severity -> MockT m ()
setUnmetExpectationCheck severity = fromMockSetup $ do
  state <- MockSetup ask
  mockSetupSTM $ writeTVar (mockUnmetSeverity state) severity

-- | Fetches a 'String' that describes the current set of outstanding
-- expectations.  This is sometimes useful for debugging test code.  The exact
-- format is not specified.
describeExpectations :: MonadIO m => MockT m String
describeExpectations = fromMockSetup $ do
  states <- allStates <$> MockSetup ask
  expectSets <- mapM (mockSetupSTM . readTVar . mockExpectSet) states
  return $
    intercalate "\n----- (next layer) -----\n" $
      formatExpectSet <$> expectSets

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
verifyExpectations = join $ do
  fromMockSetup $ do
    states <- MockSetup ask
    expectSet <- mockSetupSTM $ readTVar $ mockExpectSet states
    missingSev <- mockSetupSTM $ readTVar $ mockUnmetSeverity states
    case excess expectSet of
      ExpectNothing -> return (return ())
      missing ->
        return $
          reportFault missingSev $
            "Unmet expectations:\n" ++ formatExpectSet missing

-- | Adds a handler for unexpected actions.  Matching calls will not fail, but
-- will use a default response instead.  The rule passed in must have zero or
-- one responses: if there is a response, @'allowUnexpected' (m
-- 'Test.HMock.Rule.|=>' r)@ is equivalent to @'allowUnexpected' m >>
-- 'byDefault' (m 'Test.HMock.Rule.|=>' r)@.
--
-- The difference between 'Test.HMock.Expectable.expectAny' and
-- 'allowUnexpected' is subtle, but comes down to ambiguity:
--
-- * 'allowUnexpected' is not an expectation, so it cannot be ambiguous.  It
--   only has an effect if no true expectation matches, regardless of when the
--   expectations were added.
-- * 'Test.HMock.Expectable.expectAny' adds an expectation, so if another
--   expectation is in effect at the same time, a call to the method is
--   ambiguous.  If ambiguity checking is enabled, the method will throw an
--   error; otherwise, the more recently added of the two expectations is used.
allowUnexpected ::
  forall cls name m r rule ctx.
  ( MonadIO m,
    MockableMethod cls name m r,
    Expectable cls name m r rule,
    MockContext ctx
  ) =>
  rule ->
  ctx m ()
allowUnexpected e = fromMockSetup $ case toRule e of
  _ :=> (_ : _ : _) -> error "allowUnexpected may not have multiple responses."
  m :=> r -> do
    initClassIfNeeded (Proxy :: Proxy cls)
    state <- MockSetup ask
    mockSetupSTM $
      modifyTVar'
        (mockAllowUnexpected state)
        (Step (locate callStack (m :-> listToMaybe r)) :)

-- | Sets a default action for *expected* matching calls.  The new default only
-- applies to calls for which an expectation exists, but it lacks an explicit
-- response.  The rule passed in must have exactly one response.
byDefault ::
  forall cls name m r ctx.
  ( MonadIO m,
    MockableMethod cls name m r,
    MockContext ctx
  ) =>
  Rule cls name m r ->
  ctx m ()
byDefault (m :=> [r]) = fromMockSetup $ do
  initClassIfNeeded (Proxy :: Proxy cls)
  state <- MockSetup ask
  mockSetupSTM $
    modifyTVar'
      (mockDefaults state)
      (Step (locate callStack (m :-> Just r)) :)
byDefault _ = error "Defaults must have exactly one response."

-- | Adds a side-effect, which happens whenever a matching call occurs, in
-- addition to the usual response.  The return value is entirely ignored.
--
-- Be warned: using side effects makes it easy to break abstraction boundaries.
-- Be aware that there may be other uses of a method besides the one which you
-- intend to intercept here.  If possible, add the desired behavior to the
-- response for the matching expectation instead.
whenever ::
  forall cls name m r ctx.
  ( MonadIO m,
    MockableMethod cls name m r,
    MockContext ctx
  ) =>
  Rule cls name m r ->
  ctx m ()
whenever (m :=> [r]) = fromMockSetup $ do
  initClassIfNeeded (Proxy :: Proxy cls)
  state <- MockSetup ask
  mockSetupSTM $
    modifyTVar'
      (mockSideEffects state)
      (Step (locate callStack (m :-> Just r)) :)
whenever _ = error "Side effects must have exactly one response."
