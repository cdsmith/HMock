-- | This module contains the symbols you need to write a 'Mockable' instance
-- for a new 'Monad' type.
module HMock.Mockable (module HMock.Internal.Core) where

import HMock.Internal.Core
  ( MatchResult (..),
    Mockable (..),
    mockAction,
  )