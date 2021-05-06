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

module Classes where

import Data.Dynamic
import HMock
import HMock.Mockable
import HMock.TH

class MonadSimple m where
  simple :: String -> m ()

makeMockable ''MonadSimple

class MonadMPTC a m where
  mptc :: a -> m ()

makeMockable ''MonadMPTC

class MonadFDSpec a m | m -> a where
  fdSpec :: a -> m ()

makeMockableType [t| MonadFDSpec String |]

class MonadFDGen a m | m -> a where
  fdGen :: a -> m ()

deriveMockable ''MonadFDGen

newtype MyBase m a = MyBase {runMyBase :: m a}
  deriving (Functor, Applicative, Monad)

instance (Monad m, Typeable m) => MonadFDGen Int (MockT (MyBase m)) where
  fdGen x = mockMethod (FdGen x)

class MonadPolyArg m where
  polyArg :: Enum a => String -> a -> b -> m ()

makeMockable ''MonadPolyArg

class MonadUnshowable m where
  unshowableArgs :: (Int -> Int) -> m Int

makeMockable ''MonadUnshowable

class MonadInArg m where
  monadInArg :: (Int -> m ()) -> m ()

makeMockable ''MonadInArg

class MonadExtraneousMembers m where
  data SomeDataType m
  favoriteNumber :: SomeDataType m -> Int

  mockableMethod :: Int -> m ()

deriveMockable ''MonadExtraneousMembers

instance (Typeable m, Monad m) => MonadExtraneousMembers (MockT m) where
  data SomeDataType (MockT m) = SomeCon
  favoriteNumber SomeCon = 42
  mockableMethod a = mockMethod (MockableMethod a)
