{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DataKinds #-}
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

deriveMockable [t|MonadFoo2|]

newtype MyTestT m a = MyTestT {runMyTestT :: m a}
  deriving (Functor, Applicative, Monad)

instance (Monad m, Typeable m) => MonadFoo2 Int (MockT (MyTestT m)) where
  foo2 x = mockMethod (Foo2 x)

class MonadFoo3 m where
  foo3 :: Enum a => String -> a -> b -> m ()

makeMockable [t|MonadFoo3|]

class MonadFoo4 m where
    foo :: (Int -> m ()) -> m ()

makeMockable [t|MonadFoo4|]
