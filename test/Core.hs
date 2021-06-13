{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Core where

import Control.DeepSeq (rnf)
import Control.Exception (SomeException, evaluate)
import Control.Monad (replicateM_)
import Control.Monad.State (execStateT, modify)
import Control.Monad.Trans (liftIO)
import Data.List (isInfixOf, isPrefixOf)
import Test.HMock
import Test.HMock.TH (makeMockable)
import Test.Hspec
import Prelude hiding (readFile, writeFile)
import qualified Prelude

class Monad m => MonadFilesystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

-- | This is not used by tests.  It's just an illustration of how you'd use
-- 'MonadFilesystem' in production.
instance MonadFilesystem IO where
  readFile = Prelude.readFile
  writeFile = Prelude.writeFile

makeMockable ''MonadFilesystem

newtype SocketHandle = Handle Int deriving (Eq, Show)

class Monad m => MonadSocket m where
  openSocket :: Int -> m SocketHandle
  closeSocket :: SocketHandle -> m ()

makeMockable ''MonadSocket

coreTests :: SpecWith ()
coreTests = do
  describe "HMock core" $ do
    it "verifies a file copy" $
      example $ do
        let copyFile :: MonadFilesystem m => FilePath -> FilePath -> m ()
            copyFile a b = readFile a >>= writeFile b

        runMockT $ do
          expect $ ReadFile "foo.txt" |-> "lorem ipsum"
          expect $ WriteFile "bar.txt" "lorem ipsum"

          copyFile "foo.txt" "bar.txt"

    it "rejects an incorrect file copy" $
      example $ do
        let badCopyFile :: MonadFilesystem m => FilePath -> FilePath -> m ()
            badCopyFile a b = readFile b >>= writeFile a

            failure = runMockT $ do
              expect $ ReadFile "foo.txt" |-> "lorem ipsum"
              expect $ WriteFile "bar.txt" "lorem ipsum"

              badCopyFile "foo.txt" "bar.txt"

        failure `shouldThrow` anyException

    it "tracks expectations across multiple classes" $
      example $ do
        let setExpectations =
              inSequence
                [ expect $ ReadFile "code.txt" |-> "alpha",
                  expect $ OpenSocket 80 |-> Handle 80,
                  expect $ WriteFile "code.txt" "alpha+",
                  expect $ CloseSocket (Handle 80)
                ]

            success = runMockT $ do
              setExpectations

              code <- readFile "code.txt"
              h <- openSocket 80
              writeFile "code.txt" (code ++ "+")
              closeSocket h

            failure = runMockT $ do
              setExpectations

              h <- openSocket 80
              code <- readFile "code.txt"
              closeSocket h
              writeFile "code.txt" (code ++ "+")

        success

        failure
          `shouldThrow` errorWith ("Unexpected action: openSocket" `isInfixOf`)

    it "catches unmet expectations" $
      example $ do
        let test = runMockT $ do
              expect $ WriteFile "bar.txt" "bar"

              -- Don't write the file.
              return ()

        test
          `shouldThrow` errorWith
            (("Unmet expectations" `isInfixOf`) <&&> ("Core.hs:" `isInfixOf`))

    it "catches partially unmet expectations" $
      example $ do
        let test = runMockT $ do
              expect $ WriteFile "foo.txt" "foo"
              expect $ WriteFile "bar.txt" "bar"

              writeFile "foo.txt" "foo"

        test `shouldThrow` errorWith ("Unmet expectations" `isInfixOf`)

    it "catches partially unmet sequences" $
      example $ do
        let test = runMockT $ do
              inSequence
                [ expect $ WriteFile "foo.txt" "foo",
                  expect $ WriteFile "bar.txt" "bar",
                  expect $ WriteFile "baz.txt" "baz"
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
              expect $ WriteFile "bar.txt" "bar"
              writeFile "bar.txt" "incorrect"

        test
          `shouldThrow` errorWith
            (("Wrong arguments" `isInfixOf`) <&&> ("Core.hs:" `isInfixOf`))

    it "matches with imprecise predicates" $
      example . runMockT $ do
        expect $ WriteFile_ (hasSubstr "bar") anything
        writeFile "bar.txt" "unknown contents"

    it "stores source location in suchThat predicate" $
      example $ do
        let test = runMockT $ do
              expect $ ReadFile_ (is ("foo" `isPrefixOf`)) |-> "foo"
              readFile "bar.txt"

        test `shouldThrow` errorWith ("Core.hs" `isInfixOf`)

    it "prefers most recent method match" $
      example . runMockT $ do
        whenever $ ReadFile "foo.txt" |-> "a"
        whenever $ ReadFile "foo.txt" |-> "b"
        expect $ ReadFile "foo.txt" |-> "c"
        expect $ ReadFile "foo.txt" |-> "d"

        r1 <- readFile "foo.txt"
        r2 <- readFile "foo.txt"
        r3 <- readFile "foo.txt"
        r4 <- readFile "foo.txt"

        liftIO $ r1 `shouldBe` "d"
        liftIO $ r2 `shouldBe` "c"
        liftIO $ r3 `shouldBe` "b"
        liftIO $ r4 `shouldBe` "b"

    it "matches flexible multiplicity" $
      example $ do
        let setExpectations = do
              expectN (atLeast 3) $ ReadFile "foo.txt" |-> "foo"
              expectN (atMost 2) $ ReadFile "bar.txt" |-> "bar"
              whenever $ ReadFile "baz.txt" |-> "baz"

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
              inSequence
                [ inAnyOrder
                    [ expect $ ReadFile "1.txt" |-> "1",
                      expect $ ReadFile "2.txt" |-> "2"
                    ],
                  expect $ ReadFile "3.txt" |-> "3"
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
        inSequence
          [ inSequence
              [ expect $ ReadFile "a" |-> "a",
                expect $ ReadFile "b" |-> "b"
              ],
            expect $ ReadFile "c" |-> "c"
          ]

        _ <- readFile "a"
        _ <- readFile "b"
        _ <- readFile "c"
        return ()

    it "consumes optional calls in sequences" $
      example $ do
        let setExpectations =
              inSequence
                [ whenever $ WriteFile "foo.txt" "foo",
                  whenever $ WriteFile "foo.txt" "bar"
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
              expect $
                ReadFile_ anything
                  |=> \(ReadFile f) -> return ("contents of " ++ f)

              readFile "foo.txt"

        test `shouldReturn` "contents of foo.txt"

    it "allows responses to run in the underlying monad" $
      example $ do
        filesRead <- flip execStateT (0 :: Int) $
          runMockT $ do
            whenever $
              ReadFile_ anything
                |=> \_ -> modify (+ 1) >> return ""

            _ <- readFile "foo.txt"
            _ <- readFile "bar.txt"
            return ()

        filesRead `shouldBe` 2

    it "respects expectations added by a response" $
      example $ do
        let setExpectations = do
              whenever $
                OpenSocket_ anything |=> \(OpenSocket n) -> do
                  expect $ CloseSocket (Handle n)
                  return (Handle n)

            success = runMockT $ do
              setExpectations

              h <- openSocket 80
              closeSocket h

            failure = runMockT $ do
              setExpectations

              _ <- openSocket 80
              return ()

        success
        failure `shouldThrow` anyException

    it "describes expectations when asked" $
      example . runMockT $ do
        expectations <- describeExpectations

        -- Format is deliberately unspecified.  We're forcing it here so that
        -- test coverage doesn't flag the formatting code as untested.
        liftIO $ evaluate (rnf expectations)

    it "verifies expectations early" $
      example $ do
        let test = runMockT $ do
              expect $ ReadFile "foo.txt" |-> "lorem ipsum"
              verifyExpectations
              _ <- readFile "foo.txt"
              return ()

        test `shouldThrow` anyException

errorWith :: (String -> Bool) -> SomeException -> Bool
errorWith p e = p (show e)

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
x <&&> y = (&&) <$> x <*> y
