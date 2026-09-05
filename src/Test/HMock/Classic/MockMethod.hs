{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions to delegate 'Test.HMock.Classic.Action's to HMock to match
-- expectations.  There is one delegation function that works if the return
-- type has a 'Default' instance, and another that doesn't require the
-- 'Default' instance, but causes the method to return 'undefined' by default.
module Test.HMock.Classic.MockMethod
  ( mockMethod,
    mockDefaultlessMethod,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Default (Default)
import Data.Proxy (Proxy (Proxy))
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Test.HMock.Classic.Internal
  ( MockEnv (..),
    MockT,
    MockableMethod,
    initClassIfNeeded,
    runMockSetupWith,
    withMockEnv,
  )
import Test.HMock.Internal.Match (mockDefaultlessMethodWith, mockMethodWith)
import Test.HMock.Mockable (MockableBase (Action))
import Test.HMock.Internal.State (MockState)

-- | Runs the class's setup if needed, then hands the action to the engine.
delegate ::
  forall cls name m r.
  (MonadIO m, MockableMethod cls name m r) =>
  (MockState m -> Action (cls m) name r -> m r) ->
  Action (cls m) name r ->
  MockT m r
delegate engine action = withMockEnv $ \env -> do
  runMockSetupWith (initClassIfNeeded (Proxy :: Proxy cls)) env
  engine (mockEnvState env) action

-- | Implements a method in a 'Test.HMock.Classic.MockT.Mockable' monad by
-- delegating to the mock framework.  If the method is called unexpectedly, an
-- exception will be thrown.  However, an expected invocation without a
-- specified response will return the default value.
mockMethod ::
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r,
    Default r
  ) =>
  Action (cls m) name r ->
  MockT m r
mockMethod = withFrozenCallStack $ delegate (\st -> mockMethodWith st Nothing)

-- | Implements a method in a 'Test.HMock.Classic.MockT.Mockable' monad by
-- delegating to the mock framework.  If the method is called unexpectedly, an
-- exception will be thrown.  However, an expected invocation without a
-- specified response will return undefined.  This can be used in place of
-- 'mockMethod' when the return type has no default.
mockDefaultlessMethod ::
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r
  ) =>
  Action (cls m) name r ->
  MockT m r
mockDefaultlessMethod =
  withFrozenCallStack $ delegate (\st -> mockDefaultlessMethodWith st Nothing)
