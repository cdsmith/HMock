-- | This module provides Template Haskell splices that can be used to derive
-- boilerplate instances for HMock.
module Test.HMock.TH
  ( makeMockable,
    makeMockableWithOptions,
    deriveMockable,
    deriveMockableWithOptions,
    deriveForMockT,
    deriveForMockTWithOptions,
    makeMockableType,
    makeMockableTypeWithOptions,
    deriveMockableType,
    deriveMockableTypeWithOptions,
    deriveTypeForMockT,
    deriveTypeForMockTWithOptions,
    MockableOptions (..),
  )
where

import Test.HMock.Internal.TH
