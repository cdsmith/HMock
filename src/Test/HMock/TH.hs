{- |

This module provides Template Haskell splices that can be used to derive
boilerplate instances for HMock.

There are 12 splices described here, based on all combinations of three choices:

* Whether to generate a 'Test.HMock.Mockable' instance, an instance for
  'Test.HMock.MockT', or both.
* Whether the argument is a class name, or a type which may be partially applied
  to concrete arguments.
* Whether options are passed to customize the behavior.
-}

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
