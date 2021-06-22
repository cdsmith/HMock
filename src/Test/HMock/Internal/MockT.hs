{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the 'MockT' monad transformer.
module Test.HMock.Internal.MockT where

import Control.Monad (forM_, join, unless)
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
import Data.Functor (($>))
import Data.List (intercalate, sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (TypeRep, cast, typeRep)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Test.HMock.ExpectContext (ExpectContext (..), MockableMethod)
import Test.HMock.Internal.ExpectSet
import Test.HMock.Internal.Rule (Rule ((:=>)))
import Test.HMock.Internal.Step (SingleRule (..), Step (..), unwrapExpected)
import Test.HMock.Internal.Util (Located (..), locate, withLoc)
import Test.HMock.Mockable (MatchResult (..), Mockable (..), MockableBase (..))
import Test.HMock.Rule (Expectable (..))
import UnliftIO

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

-- | Monad for setting up a mockable class.  Note that even though the type
-- looks that way, this is *not* a monad transformer.  It's a very restricted
-- environment that can only be used to set up defaults for the class.
newtype MockSetupT m a where
  MockSetupT :: {unMockSetupT :: ReaderT (MockState m) STM a} -> MockSetupT m a
  deriving (Functor, Applicative, Monad)

-- | Run an STM action in 'MockSetupT'
mockSetupSTM :: STM a -> MockSetupT m a
mockSetupSTM m = MockSetupT (lift m)

-- | Type class for contexts in which defaults can be set up for a mockable
-- class.  Notably, this includes `MockSetupT` and `MockT`.
class MockSetupContext ctx where
  -- | Runs a 'MockSetupT' action in this monad.
  fromMockSetup :: MonadIO m => MockSetupT m a -> ctx m a

instance MockSetupContext MockT where
  fromMockSetup m = do
    state <- MockT ask
    atomically $ runReaderT (unMockSetupT m) state

instance MockSetupContext MockSetupT where
  fromMockSetup = id

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

-- | Adds an expectation to the 'MockState' for the given 'ExpectSet',
-- interleaved with any existing expectations.
expectThisSet :: HasCallStack => MonadIO m => ExpectSet (Step m) -> MockT m ()
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

-- | Sets a default action for matching calls.  These calls will not fail, but
-- the default will only be used if there is no expectation in effect with an
-- explicit response.  The rule passed in must have exactly one response.
--
-- The difference between 'expectAny' and 'byDefault' is subtle.  It comes down
-- to ambiguity:
--
-- * 'byDefault' is not an expectation.  It only has an effect if no expectation
--   matches, regardless of when the expectations were added.
-- * 'expectAny' adds an expectation, so if another expectation is in effect at
--   the same time, a call to the method is ambiguous.  If ambiguity checking is
--   enabled, the method will throw an error; otherwise, the more recently added
--   of the two expectations is used.
--
-- Without 'byDefault', actions with no explicit response will return the
-- 'Default' value for the type (or 'undefined' if the return type isn't an
-- instance of 'Default').
byDefault ::
  forall cls name m r rule ctx.
  ( MonadIO m,
    MockableMethod cls name m r,
    Expectable cls name m r rule,
    MockSetupContext ctx
  ) =>
  rule ->
  ctx m ()
byDefault e | m :=> [r] <- toRule e = fromMockSetup $ do
  initClassIfNeeded (Proxy :: Proxy cls)
  state <- MockSetupT ask
  mockSetupSTM $
    modifyTVar'
      (mockDefaults state)
      ((True, Step (locate callStack (m :-> Just r))) :)
byDefault _ = error "Defaults must have exactly one response."

-- | Sets a default action for *expected* matching calls.  The new default only
-- applies to calls for which an expectation exists, but it lacks an explicit
-- response.  The rule passed in must have exactly one response.
setDefault ::
  forall cls name m r rule ctx.
  ( MonadIO m,
    MockableMethod cls name m r,
    Expectable cls name m r rule,
    MockSetupContext ctx
  ) =>
  rule ->
  ctx m ()
setDefault e | m :=> [r] <- toRule e = fromMockSetup $ do
  initClassIfNeeded (Proxy :: Proxy cls)
  state <- MockSetupT ask
  mockSetupSTM $
    modifyTVar'
      (mockDefaults state)
      ((False, Step (locate callStack (m :-> Just r))) :)
setDefault _ = error "Defaults must have exactly one response."

-- | Sets whether to check for ambiguous actions.  If 'True', then actions that
-- match expectations in more than one way will fail.  If 'False', then the
-- most recently added action will take precedence.  This defaults to 'False'.
setAmbiguityCheck :: MonadIO m => Bool -> MockT m ()
setAmbiguityCheck flag =
  MockT ask >>= atomically . flip writeTVar flag . mockCheckAmbiguity

-- | Implements mock delegation for actions.
mockMethodImpl ::
  forall cls name m r.
  (HasCallStack, MonadIO m, MockableMethod cls name m r) =>
  r ->
  Action cls name m r ->
  MockT m r
mockMethodImpl surrogate action = join $
  fromMockSetup $ do
    initClassIfNeeded (Proxy :: Proxy cls)
    state <- MockSetupT ask
    expectSet <- mockSetupSTM $ readTVar (mockExpectSet state)
    defaults <- mockSetupSTM $ readTVar (mockDefaults state)
    checkAmbig <- mockSetupSTM $ readTVar (mockCheckAmbiguity state)
    let (newExpectSet, response) =
          decideAction expectSet defaults checkAmbig
    mockSetupSTM $ writeTVar (mockExpectSet state) newExpectSet
    return response
  where
    decideAction ::
      ExpectSet (Step m) ->
      [(Bool, Step m)] ->
      Bool ->
      (ExpectSet (Step m), MockT m r)
    decideAction expectSet defaults checkAmbig =
      let (partial, full) = partitionEithers (tryMatch <$> liveSteps expectSet)
          orderedPartial = snd <$> sortBy (compare `on` fst) (catMaybes partial)
       in case (full, surrogate, orderedPartial) of
            (opts@(_ : _ : _), _, _)
              | checkAmbig ->
                error $
                  ambiguityError action ((\(_, s, _) -> s) <$> opts) expectSet
            ((e, _, Just response) : _, _, _) -> (e, response)
            ((e, _, Nothing) : _, s, _)
              | d <- findDefault False defaults -> (e, fromMaybe (return s) d)
            ([], _, _)
              | Just d <- findDefault True defaults -> (expectSet, d)
            ([], _, []) -> error $ noMatchError action expectSet
            ([], _, _) ->
              error $ partialMatchError action orderedPartial expectSet

    tryMatch ::
      (Step m, ExpectSet (Step m)) ->
      Either
        (Maybe (Int, String))
        (ExpectSet (Step m), String, Maybe (MockT m r))
    tryMatch (Step expected, e)
      | Just lrule@(Loc _ (m :-> impl)) <- cast expected =
        case matchAction m action of
          NoMatch n ->
            Left (Just (n, withLoc (showMatcher (Just action) m <$ lrule)))
          Match ->
            Right
              ( e,
                withLoc (lrule $> showMatcher (Just action) m),
                ($ action) <$> impl
              )
      | otherwise = Left Nothing

    findDefault :: Bool -> [(Bool, Step m)] -> Maybe (MockT m r)
    findDefault _ [] = Nothing
    findDefault unexpected ((lax, Step expected) : _)
      | lax || not unexpected,
        Just (Loc _ (m :-> Just r)) <- cast expected,
        Match <- matchAction m action =
        Just (r action)
    findDefault unexpected (_ : steps) = findDefault unexpected steps

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

-- | An error for an action that matches no expectations at all.
noMatchError ::
  Mockable cls => Action cls name m a -> ExpectSet (Step m) -> String
noMatchError a fullExpectations =
  "Unexpected action: " ++ showAction a
    ++ "\n\nFull expectations:\n"
    ++ formatExpectSet fullExpectations

-- | An error for an action that doesn't match the argument predicates for any
-- of the method's expectations.
partialMatchError ::
  Mockable cls =>
  Action cls name m a ->
  [String] ->
  ExpectSet (Step m) ->
  String
partialMatchError a partials fullExpectations =
  "Wrong arguments: "
    ++ showAction a
    ++ "\n\nClosest matches:\n - "
    ++ intercalate "\n - " (take 5 partials)
    ++ "\n\nFull expectations:\n"
    ++ formatExpectSet fullExpectations

-- | An error for an 'Action' that matches more than one 'Matcher'.  This only
-- triggers an error if ambiguity checks are on.
ambiguityError ::
  Mockable cls =>
  Action cls name m a ->
  [String] ->
  ExpectSet (Step m) ->
  String
ambiguityError a choices fullExpectations =
  "Ambiguous action matched multiple expectations: "
    ++ showAction a
    ++ "\n\nMatches:\n - "
    ++ intercalate "\n - " choices
    ++ "\n\nFull expectations:\n"
    ++ formatExpectSet fullExpectations
