-- | This module contains the symbols you need to write a 'Mockable' instance
-- for a new 'Monad' type.
module Test.HMock.Mockable (module Test.HMock.Internal.Core) where

import Test.HMock.Internal.Core
  ( MatchResult (..),
    Mockable (..),
    mockMethod,
  )