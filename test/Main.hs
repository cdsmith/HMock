{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Typeable
import HMock
import HMock.Mockable
import HMock.TH
import Test.Hspec hiding (Expectation)
import Prelude hiding (readFile, writeFile)
import qualified Prelude

class Monad m => MonadFS m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

instance MonadFS IO where
  readFile = Prelude.readFile
  writeFile = Prelude.writeFile

makeMockable [t|MonadFS|]

copyFile :: MonadFS m => FilePath -> FilePath -> m ()
copyFile a b = readFile a >>= writeFile b

class MonadExtraneousMembers m where
  data SomeDataType m
  favoriteNumber :: SomeDataType m -> Int

  mockableMethod :: Int -> m ()

deriveMockable [t|MonadExtraneousMembers|]

instance (Typeable m, Monad m) => MonadExtraneousMembers (MockT m) where
  data SomeDataType (MockT m) = FooCon
  favoriteNumber _ = 42
  mockableMethod a = mockMethod (MockableMethod a)

class MonadMultiParam a m | m -> a where
  multiParamMethod :: String -> m ()

deriveMockable [t|MonadMultiParam String|]

main :: IO ()
main = hspec $ do
  describe "MonadFS" $ do
    it "copies a file" $
      example . runMockT $ do
        mock $ expect $ ReadFile "foo.txt" |-> "lorem ipsum"
        mock $ expect $ WriteFile "bar.txt" "lorem ipsum" |-> ()

        copyFile "foo.txt" "bar.txt"

  describe "MonadExtraneousMembers" $ do
    it "mocks mockableMethod" $
      example . runMockT $ do
        mock $ expect $ MockableMethod 42 |-> ()
        mockableMethod 42
