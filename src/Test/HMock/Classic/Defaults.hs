{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Behaviors that apply to matching calls whenever no expectation says
-- otherwise.  These are the functions you use from 'Test.HMock.Classic.MockT.setupMockable' to give a
-- class its defaults, though they work from a test just as well.
module Test.HMock.Classic.Defaults
  ( allowUnexpected,
    byDefault,
    whenever,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (listToMaybe)
import Test.HMock.Classic.Internal
  ( Expectable (toRule),
    MockContext (fromMockSetup),
    MockSetup,
    MockableMethod,
    Rule ((:=>)),
    liftMockSTM,
    prepareRule,
  )
import qualified Test.HMock.Internal.Rule as Core
import Test.HMock.Internal.State (MockState (..), askState, liftSTM)
import Test.HMock.Internal.Step (SingleRule ((:->)), Step (Step))
import Test.HMock.Internal.Util (Located (Loc))
import UnliftIO (TVar, modifyTVar')

-- | Installs a rule with at most one response into one of the state's lists
-- of standing rules.  Standing rules never appear in messages, so they carry
-- no source location.
addRule ::
  forall cls name m r.
  (MonadIO m, MockableMethod cls name m r) =>
  (MockState m -> TVar [Step m]) ->
  Rule cls name m r ->
  MockSetup m ()
addRule field rule = do
  core <- prepareRule rule
  case core of
    m Core.:=> rs -> liftMockSTM $ do
      state <- askState
      liftSTM $
        modifyTVar'
          (field state)
          (Step Nothing (Loc Nothing (m :-> listToMaybe rs)) :)

-- | Adds a handler for unexpected actions.  Matching calls will not fail, but
-- will use a default response instead.  The rule passed in must have zero or
-- one responses.  If there is a response, it is used for unexpected calls in
-- preference to any 'byDefault' rule; otherwise the 'byDefault' rule applies.
-- Expected calls never use the response given here.
--
-- The difference between 'Test.HMock.Classic.ExpectContext.expectAny' and
-- 'allowUnexpected' is subtle, but comes down to ambiguity:
--
-- * 'allowUnexpected' is not an expectation, so it cannot be ambiguous.  It
--   only has an effect if no true expectation matches, regardless of when the
--   expectations were added.
-- * 'Test.HMock.Classic.ExpectContext.expectAny' adds an expectation, so if
--   another expectation is in effect at the same time, a call to the method is
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
allowUnexpected e = case toRule e of
  _ :=> (_ : _ : _) -> error "allowUnexpected may not have multiple responses."
  rule -> fromMockSetup (addRule mockAllowUnexpected rule)

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
byDefault rule@(_ :=> [_]) = fromMockSetup (addRule mockDefaults rule)
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
whenever rule@(_ :=> [_]) = fromMockSetup (addRule mockSideEffects rule)
whenever _ = error "Side effects must have exactly one response."
