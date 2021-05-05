{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TH where

import Data.Dynamic
import HMock
import HMock.Mockable
import HMock.TH
import Test.Hspec

class MonadFoo1 m where
  foo1 :: String -> m ()

makeMockable ''MonadFoo1

class MonadFoo2 a m | m -> a where
  foo2 :: a -> m ()

deriveMockable ''MonadFoo2

newtype MyTestT m a = MyTestT {runMyTestT :: m a}
  deriving (Functor, Applicative, Monad)

instance (Monad m, Typeable m) => MonadFoo2 Int (MockT (MyTestT m)) where
  foo2 x = mockMethod (Foo2 x)

class MonadFoo3 m where
  foo3 :: Enum a => String -> a -> b -> m ()

makeMockable ''MonadFoo3

class MonadFoo4 m where
  foo :: (Int -> m ()) -> m ()

makeMockable ''MonadFoo4

class MonadExtraneousMembers m where
  data SomeDataType m
  favoriteNumber :: SomeDataType m -> Int

  mockableMethod :: Int -> m ()

deriveMockable ''MonadExtraneousMembers

instance (Typeable m, Monad m) => MonadExtraneousMembers (MockT m) where
  data SomeDataType (MockT m) = FooCon
  favoriteNumber _ = 42
  mockableMethod a = mockMethod (MockableMethod a)

class MonadMultiParam a m | m -> a where
  multiParamMethod :: a -> m ()

deriveMockableType [t|MonadMultiParam String|]

class MonadUnshowable m where
  unshowableArgs :: (Int -> Int) -> m Int

makeMockable ''MonadUnshowable

thTests :: SpecWith ()
thTests = return ()