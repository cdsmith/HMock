-- | This module provides Template Haskell splices that can be used to derive
-- boilerplate instances for HMock.
--
-- There are 20 splices described here, based on combinations of four
-- choices:
--
-- * Whether to generate a 'Test.HMock.MockableBase', an instance for
--   'Test.HMock.MockT', or both.
-- * When generating 'Test.HMock.MockableBase', whether to also generate a
--   'Test.HMock.Mockable' instance with an empty setup.
-- * Whether the argument is a class name, or a type which may be partially
--   applied to concrete arguments.
-- * Whether options are passed to customize the behavior.
module Test.HMock.TH
  ( makeMockable,
    makeMockableWithOptions,
    makeMockableBase,
    makeMockableBaseWithOptions,
    deriveMockable,
    deriveMockableWithOptions,
    deriveMockableBase,
    deriveMockableBaseWithOptions,
    deriveForMockT,
    deriveForMockTWithOptions,
    makeMockableType,
    makeMockableTypeWithOptions,
    makeMockableBaseType,
    makeMockableBaseTypeWithOptions,
    deriveMockableType,
    deriveMockableTypeWithOptions,
    deriveMockableBaseType,
    deriveMockableBaseTypeWithOptions,
    deriveTypeForMockT,
    deriveTypeForMockTWithOptions,
    MockableOptions (..),
  )
where

import Test.HMock.Internal.TH
