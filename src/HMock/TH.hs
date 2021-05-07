-- | This module provides Template Haskell splices that can be used to derive
-- boilerplate instances for HMock.
module HMock.TH
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

import HMock.Internal.TH
