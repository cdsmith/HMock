{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module TH where

import Data.Default
import Data.Dynamic
import Data.Kind
import Data.Typeable
import HMock
import HMock.Mockable
import HMock.TH

class MonadFoo1 m where
  foo1 :: String -> m ()

makeMockable [t|MonadFoo1|]

class MonadFoo2 a m where
  foo2 :: a -> m ()

makeMockableWithOptions (def {mockPrefix = "Int"}) [t|MonadFoo2 Int|]

class MonadFoo3 m where
  foo3 :: Enum a => String -> a -> b -> m ()

makeMockable [t|MonadFoo3|]
