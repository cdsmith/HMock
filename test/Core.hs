{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Core where

import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.List
import Test.HMock
import Test.HMock.TH
import Test.Hspec
import Prelude hiding (readFile, writeFile)

class Monad m => MonadFilesystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

makeMockable ''MonadFilesystem

class Monad m => MonadDB a m where
  storeDB :: String -> a -> m ()
  lookupDB :: String -> m a

makeMockable ''MonadDB

errorWith :: (String -> Bool) -> SomeException -> Bool
errorWith p e = p (show e)

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
x <&&> y = (&&) <$> x <*> y

coreTests :: SpecWith ()
coreTests = do
  describe "HMock core" $ do
    it "verifies a file copy" $
      example $ do
        let copyFile :: MonadFilesystem m => FilePath -> FilePath -> m ()
            copyFile a b = readFile a >>= writeFile b

            badCopyFile :: MonadFilesystem m => FilePath -> FilePath -> m ()
            badCopyFile a b = readFile b >>= writeFile a

            setExpectations = do
              mock $ expect $ readFile_ "foo.txt" |-> "lorem ipsum"
              mock $ expect $ writeFile_ "bar.txt" "lorem ipsum" |-> ()

            success = runMockT $ do
              setExpectations
              copyFile "foo.txt" "bar.txt"

            failure = runMockT $ do
              setExpectations
              badCopyFile "foo.txt" "bar.txt"

        success
        failure `shouldThrow` anyException

    it "tracks expectations across multiple classes" $
      example $ do
        let setExpectations =
              mock $
                inSequence
                  [ expect $ readFile_ "key.txt" |-> "alpha",
                    expect $ lookupDB_ "alpha" |-> "beta",
                    expect $ writeFile_ "key.txt" "beta" |-> (),
                    expect $ storeDB_ "alpha" "newVal" |-> ()
                  ]

            success = runMockT $ do
              setExpectations
              key <- readFile "key.txt"
              val <- lookupDB key
              writeFile "key.txt" val
              storeDB key "newVal"

            failure = runMockT $ do
              setExpectations
              key <- readFile "key.txt"
              val <- lookupDB key
              storeDB key "newVal"
              writeFile "key.txt" val

        success

        failure
          `shouldThrow` errorWith ("Unexpected action: storeDB" `isInfixOf`)

    it "catches unmet expectations" $
      example $
        runMockT (mock $ expect $ writeFile_ "bar.txt" "bar" |-> ())
          `shouldThrow` errorWith
            (("Unmet expectations" `isInfixOf`) <&&> ("Core.hs:" `isInfixOf`))

    it "catches partially unmet expectations" $
      example $ do
        let test = runMockT $ do
              mock $ expect $ writeFile_ "foo.txt" "foo" |-> ()
              mock $ expect $ writeFile_ "bar.txt" "bar" |-> ()

              writeFile "foo.txt" "foo"
        test `shouldThrow` errorWith ("Unmet expectations" `isInfixOf`)

    it "catches partially unmet sequences" $
      example $ do
        let test = runMockT $ do
              mock $
                inSequence
                  [ expect $ writeFile_ "foo.txt" "foo" |-> (),
                    expect $ writeFile_ "bar.txt" "bar" |-> (),
                    expect $ writeFile_ "baz.txt" "baz" |-> ()
                  ]

              writeFile "foo.txt" "foo"
        test `shouldThrow` errorWith ("Unmet expectations" `isInfixOf`)

    it "catches unexpected actions" $
      example $
        runMockT (writeFile "bar.txt" "bar")
          `shouldThrow` errorWith ("Unexpected action: writeFile" `isInfixOf`)

    it "catches incorrect arguments" $
      example $ do
        let test = runMockT $ do
              mock $ expect $ writeFile_ "bar.txt" "bar" |-> ()
              writeFile "bar.txt" "incorrect"
        test
          `shouldThrow` errorWith
            (("Wrong arguments" `isInfixOf`) <&&> ("Core.hs:" `isInfixOf`))

    it "matches with imprecise predicates" $
      example . runMockT $ do
        mock $ expect $ WriteFile_ (hasSubstr "bar") anything |-> ()
        writeFile "bar.txt" "unknown contents"

    it "stores source location in suchThat predicate" $
      example $ do
        let test = runMockT $ do
              mock $
                expect $ ReadFile_ (suchThat ("foo" `isPrefixOf`)) |-> "foo"
              readFile "bar.txt"
        test `shouldThrow` errorWith ("Core.hs" `isInfixOf`)

    it "fails when responses are ambiguous" $
      example $ do
        let test = runMockT $ do
              mock $ whenever $ readFile_ "foo.txt" |-> "a"
              mock $ whenever $ readFile_ "foo.txt" |-> "b"
              readFile "foo.txt"
        test `shouldThrow` errorWith ("Ambiguous matches" `isInfixOf`)

    it "matches flexible cardinality" $
      example $ do
        let setExpectations = do
              mock $ expectN (atLeast 3) $ readFile_ "foo.txt" |-> "foo"
              mock $ expectN (atMost 2) $ readFile_ "bar.txt" |-> "bar"
              mock $ expectAny $ readFile_ "baz.txt" |-> "baz"

            success1 = runMockT $ do
              setExpectations
              replicateM_ 3 $ readFile "foo.txt"

            success2 = runMockT $ do
              setExpectations
              replicateM_ 4 $ readFile "foo.txt"

            success3 = runMockT $ do
              setExpectations
              replicateM_ 3 $ readFile "foo.txt"
              replicateM_ 2 $ readFile "bar.txt"

            success4 = runMockT $ do
              setExpectations
              replicateM_ 3 $ readFile "foo.txt"
              replicateM_ 2 $ readFile "bar.txt"
              replicateM_ 5 $ readFile "baz.txt"

            failure1 = runMockT $ do
              setExpectations
              replicateM_ 1 $ readFile "foo.txt"

            failure2 = runMockT $ do
              setExpectations
              replicateM_ 3 $ readFile "foo.txt"
              replicateM_ 3 $ readFile "bar.txt"

        success1
        success2
        success3
        success4
        failure1 `shouldThrow` anyException
        failure2 `shouldThrow` anyException

    it "enforces complex nested sequences" $
      example $ do
        let setExpectations =
              mock $
                inSequence
                  [ inAnyOrder
                      [ expect $ readFile_ "1.txt" |-> "1",
                        expect $ readFile_ "2.txt" |-> "2"
                      ],
                    expect $ readFile_ "3.txt" |-> "3"
                  ]

            success1 = runMockT $ do
              setExpectations
              _ <- readFile "1.txt"
              _ <- readFile "2.txt"
              _ <- readFile "3.txt"
              return ()

            success2 = runMockT $ do
              setExpectations
              _ <- readFile "2.txt"
              _ <- readFile "1.txt"
              _ <- readFile "3.txt"
              return ()

            failure = runMockT $ do
              setExpectations
              _ <- readFile "2.txt"
              _ <- readFile "3.txt"
              _ <- readFile "1.txt"
              return ()

        success1
        success2
        failure `shouldThrow` anyException

    it "handles nested sequences" $
      example . runMockT $ do
        mock $
          inSequence
            [ inSequence
                [ expect $ readFile_ "a" |-> "a",
                  expect $ readFile_ "b" |-> "b"
                ],
              expect $ readFile_ "c" |-> "c"
            ]
        _ <- readFile "a"
        _ <- readFile "b"
        _ <- readFile "c"
        return ()

    it "overrides default responses" $
      example $ do
        responses <- runMockT $ do
          mock $ whenever $ ReadFile_ anything |-> "default"
          mock $
            inSequence
              [ expect $ readFile_ "a.txt" |-> "A1",
                expectN (exactly 2) $ readFile_ "a.txt" |-> "A2"
              ]
          mock $ expectAny $ readFile_ "b.txt" |-> "B"

          mapM readFile (replicate 4 "a.txt" ++ replicate 2 "b.txt")
        responses `shouldBe` ["A1", "A2", "A2", "default", "B", "B"]

    it "consumes optional calls in sequences" $
      example $ do
        let setExpectations =
              mock $
                inSequence
                  [ expectAny $ writeFile_ "foo.txt" "foo" |-> (),
                    expectAny $ writeFile_ "foo.txt" "bar" |-> ()
                  ]

            success = runMockT $ do
              setExpectations
              writeFile "foo.txt" "foo"
              writeFile "foo.txt" "bar"

            failure = runMockT $ do
              setExpectations
              writeFile "foo.txt" "foo"
              writeFile "foo.txt" "bar"
              writeFile "foo.txt" "foo"

        success
        failure `shouldThrow` errorWith ("Wrong arguments:" `isInfixOf`)

    it "gives access to method arguments in the response" $
      example $ do
        let test = runMockT $ do
              mock $
                expect $
                  ReadFile_ anything
                    :-> \(ReadFile f) -> return ("contents of " ++ f)
              readFile "foo.txt"
        test `shouldReturn` "contents of foo.txt"

    it "allows responses to run in the underlying monad" $
      example $ do
        filesRead <- flip execStateT (0 :: Int) $
          runMockT $ do
            mock $
              whenever $
                ReadFile_ anything
                  :-> \_ -> modify (+ 1) >> return ""
            _ <- readFile "foo.txt"
            _ <- readFile "bar.txt"
            return ()
        filesRead `shouldBe` 2

    it "respects expectations added by a response" $
      example $ do
        result <- runMockT $ do
          mock $
            whenever $
              readFile_ "foo.txt" :-> \_ -> do
                mock $ expect $ readFile_ "bar.txt" |-> "final"
                return "bar.txt"

          readFile "foo.txt" >>= readFile

        result `shouldBe` "final"
