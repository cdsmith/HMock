{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

class MonadFoo2 a m | m -> a where
  foo2 :: a -> m ()

newtype Foo2IntM m a = Foo2IntM {runFoo2IntM :: m a}
  deriving (Functor, Applicative, Monad)

deriveMockableWithOptions def {mockPrefix = "Int"} [t|MonadFoo2 Int|]

instance (Typeable m, Monad m) => MonadFoo2 Int (MockT (Foo2IntM m)) where
  foo2 x = mockMethod (IntFoo2 x)

newtype Foo2StrM m a = Foo2StrM {runFoo2StrM :: m a}
  deriving (Functor, Applicative, Monad)

deriveMockableWithOptions def {mockPrefix = "Str"} [t|MonadFoo2 String|]

instance (Typeable m, Monad m) => MonadFoo2 String (MockT (Foo2StrM m)) where
  foo2 x = mockMethod (StrFoo2 x)

class MonadFoo3 m where
  foo3 :: Enum a => String -> a -> b -> m ()

makeMockable [t|MonadFoo3|]
