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
import TH
import Test.Hspec hiding (Expectation)
import Prelude hiding (readFile, writeFile)
import qualified Prelude

class Monad m => MonadFilesystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

instance MonadFilesystem IO where
  readFile = Prelude.readFile
  writeFile = Prelude.writeFile

makeMockable ''MonadFilesystem

copyFile :: MonadFilesystem m => FilePath -> FilePath -> m ()
copyFile a b = readFile a >>= writeFile b

main :: IO ()
main = hspec $ do
  describe "MonadFS" $ do
    it "copies a file" $
      example . runMockT $ do
        mock $ expect $ readFile_ "foo.txt" |-> "lorem ipsum"
        mock $ expect $ writeFile_ "bar.txt" "lorem ipsum" |-> ()

        copyFile "foo.txt" "bar.txt"

    it "matches flexible cardinality" $ do
      example . runMockT $ do
        mock $ expectAny $ readFile_ "foo.txt" |-> "lorem ipsum"
        mock $ expect $ writeFile_ "bar.txt" "lorem ipsum" |-> ()

        copyFile "foo.txt" "bar.txt"

    it "enforces complex nested sequences" $
      example . runMockT $ do
        mock $
          inSequence
            [ inAnyOrder
                [ expect $ readFile_ "1.txt" |-> "1",
                  expect $ readFile_ "2.txt" |-> "2"
                ],
              expect $ readFile_ "3.txt" |-> "3"
            ]

        readFile "2.txt"
        readFile "1.txt"
        readFile "3.txt"
        return ()

    it "overrides default responses" $ do
      example $ do
        (r1, r2) <- runMockT $ do
          mock $ whenever $ readFile_ "a.txt" |-> "the default"
          mock $ expect $ readFile_ "a.txt" |-> "the override"

          (,) <$> readFile "a.txt" <*> readFile "a.txt"
        r1 `shouldBe` "the override"
        r2 `shouldBe` "the default"

    it "consumes optional calls in sequences" $ do
      example . runMockT $ do
        mock $
          inSequence
            [ expectAny $ writeFile_ "foo.txt" "foo" |-> (),
              expect $ writeFile_ "foo.txt" "bar" |-> ()
            ]
        writeFile "foo.txt" "foo"
        writeFile "foo.txt" "bar"
