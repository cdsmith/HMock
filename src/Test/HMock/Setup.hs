{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains the API for configurable setup of
-- 'Test.HMock.Mockable.Mockable' classes.  By default, unexpected actions throw
-- errors, and actions with no explicit default always return the default value
-- of their return type, or 'undefined' if there is none.  You can change this
-- either on a per-class or per-test basis.
--
-- * To change defaults on a per-class basis, you should implement your own
--   'Test.HMock.Mockable.Mockable' instance and perform the setup you need
--   inside 'Test.HMock.Mockable.setupMockable'.
-- * To change defaults on a per-test basis, you should use 'byDefault' or
--   'setDefault' directly from 'Test.HMock.MockT'.
-- * Without 'byDefault' or 'setDefault', actions with no explicit response will
--   return the 'Data.Default.Default' value for the type (or 'undefined' if the
--   return type isn't an instance of 'Data.Default.Default').

module Test.HMock.Setup
  ( MockSetupT,
    MockSetupContext,
    byDefault,
    setDefault,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask)
import Data.Proxy (Proxy (..))
import GHC.Stack (callStack)
import Test.HMock.ExpectContext (MockableMethod)
import Test.HMock.Internal.Rule (Rule ((:=>)))
import Test.HMock.Internal.State
  ( MockSetupContext (..),
    MockSetupT (..),
    MockState (..),
    initClassIfNeeded,
    mockSetupSTM,
  )
import Test.HMock.Internal.Step (SingleRule (..), Step (..))
import Test.HMock.Internal.Util (locate)
import Test.HMock.Rule (Expectable (..))
import UnliftIO.STM (modifyTVar')

-- | Sets a default action for matching calls.  These calls will not fail, but
-- the default will only be used if there is no expectation in effect with an
-- explicit response.  The rule passed in must have exactly one response.
--
-- The difference between 'Test.HMock.Expectable.expectAny' and 'byDefault' is
-- subtle.  It comes down to ambiguity:
--
-- * 'byDefault' is not an expectation.  It only has an effect if no expectation
--   matches, regardless of when the expectations were added.
-- * 'Test.HMock.Expectable.expectAny' adds an expectation, so if another
--   expectation is in effect at the same time, a call to the method is
--   ambiguous.  If ambiguity checking is enabled, the method will throw an
--   error; otherwise, the more recently added of the two expectations is used.
--
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
