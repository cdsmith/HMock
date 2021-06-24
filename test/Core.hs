{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Core where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (rnf)
import Control.Exception (SomeException, evaluate)
import Control.Monad (replicateM_)
import Control.Monad.Reader (MonadReader (local), ask, runReaderT)
import Control.Monad.State (execStateT, modify)
import Control.Monad.Trans (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf)
import Test.HMock
import Test.Hspec
import qualified UnliftIO.Concurrent as UnliftIO
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

    it "uses default responses when no explicit response given" $
      example $ do
        let test = runMockT $ do
              expect $ WriteFile "file.txt" "contents"
              expect $ ReadFile "file.txt"
              writeFile "file.txt" "contents"
              readFile "file.txt"

        test `shouldReturn` ""

    it "shares expectations using withMockT" $
      example $
        withMockT $ \inMockT -> do
          expect $ ReadFile "foo.txt" |-> "lorem ipsum"
          expect $ WriteFile "bar.txt" "lorem ipsum"

          var <- liftIO newEmptyMVar

          _ <-
            liftIO $
              forkIO $ inMockT $ readFile "foo.txt" >>= liftIO . putMVar var
          writeFile "bar.txt" =<< liftIO (takeMVar var)

    it "shares expectations across threads using MonadUnliftIO" $
      example $
        runMockT $ do
          expect $ ReadFile "foo.txt" |-> "lorem ipsum"
          expect $ WriteFile "bar.txt" "lorem ipsum"

          var <- liftIO newEmptyMVar
          _ <- UnliftIO.forkIO $ readFile "foo.txt" >>= liftIO . putMVar var
          writeFile "bar.txt" =<< liftIO (takeMVar var)

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

    it "returns multiple responses" $
      example $ do
        let test = runMockT $ do
              expect $
                ReadFile "foo.txt"
                  |-> "a"
                  |-> "b"
                  |-> "c"
              (,,)
                <$> readFile "foo.txt"
                <*> readFile "foo.txt"
                <*> readFile "foo.txt"

        test `shouldReturn` ("a", "b", "c")

    it "catches expectN with too many expectations" $
      example $ do
        let test = runMockT $ do
              expectN once $
                ReadFile "foo.txt"
                  |-> "a"
                  |-> "b"

        test `shouldThrow` errorWith ("2 responses is too many" `isInfixOf`)

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
        expectAny $ ReadFile "foo.txt" |-> "a"
        expectAny $ ReadFile "foo.txt" |-> "b"
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
              expectAny $ ReadFile "baz.txt" |-> "baz"

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

    it "enforces nested sequences" $
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
                [ expectAny $ WriteFile "foo.txt" "foo",
                  expectAny $ WriteFile "foo.txt" "bar"
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

    it "implements choice" $
      example $ do
        let setExpectations =
              anyOf
                [ expect $ WriteFile "status.txt" "all systems go",
                  expect $ WriteFile "status.txt" "we have a problem"
                ]

            success1 = runMockT $ do
              setExpectations
              writeFile "status.txt" "all systems go"

            success2 = runMockT $ do
              setExpectations
              writeFile "status.txt" "we have a problem"

            failure1 = runMockT $ do
              setExpectations
              return ()

            failure2 = runMockT $ do
              setExpectations
              writeFile "status.txt" "not sure"

        success1
        success2
        failure1 `shouldThrow` anyException
        failure2 `shouldThrow` anyException

    it "implements interleaved repetition" $
      example $ do
        let setExpectations =
              times (atLeast 2) $
                inAnyOrder
                  [ expect $ WriteFile "foo.txt" "a",
                    expect $ WriteFile "bar.txt" "b"
                  ]

            success1 = runMockT $ do
              setExpectations

              writeFile "foo.txt" "a"
              writeFile "bar.txt" "b"

              writeFile "bar.txt" "b"
              writeFile "foo.txt" "a"

            success2 = runMockT $ do
              setExpectations

              writeFile "foo.txt" "a"
              writeFile "foo.txt" "a"

              writeFile "bar.txt" "b"
              writeFile "bar.txt" "b"

            tooFew = runMockT $ do
              setExpectations

              writeFile "foo.txt" "a"
              writeFile "bar.txt" "b"

            incomplete = runMockT $ do
              setExpectations

              writeFile "foo.txt" "a"
              writeFile "bar.txt" "b"

              writeFile "foo.txt" "a"
              writeFile "bar.txt" "b"

              writeFile "foo.txt" "a"

        success1
        success2
        tooFew `shouldThrow` anyException
        incomplete `shouldThrow` anyException

    it "implements consecutive repetition" $
      example $ do
        let setExpectations =
              consecutiveTimes (atLeast 2) $
                inAnyOrder
                  [ expect $ WriteFile "foo.txt" "a",
                    expect $ WriteFile "bar.txt" "b"
                  ]

            success1 = runMockT $ do
              setExpectations

              writeFile "foo.txt" "a"
              writeFile "bar.txt" "b"

              writeFile "bar.txt" "b"
              writeFile "foo.txt" "a"

            interleaved = runMockT $ do
              setExpectations

              writeFile "foo.txt" "a"
              writeFile "foo.txt" "a"

              writeFile "bar.txt" "b"
              writeFile "bar.txt" "b"

            tooFew = runMockT $ do
              setExpectations

              writeFile "foo.txt" "a"
              writeFile "bar.txt" "b"

            incomplete = runMockT $ do
              setExpectations

              writeFile "foo.txt" "a"
              writeFile "bar.txt" "b"

              writeFile "foo.txt" "a"
              writeFile "bar.txt" "b"

              writeFile "foo.txt" "a"

        success1
        interleaved `shouldThrow` anyException
        tooFew `shouldThrow` anyException
        incomplete `shouldThrow` anyException

    it "repeats response sequences during repetition" $
      example $ do
        runMockT $ do
          times 2 $ expect $ ReadFile "foo.txt" |-> "A" |-> "B"

          result <-
            (,,,) <$> readFile "foo.txt"
              <*> readFile "foo.txt"
              <*> readFile "foo.txt"
              <*> readFile "foo.txt"
          liftIO $ result `shouldBe` ("A", "B", "A", "B")

    it "repeats response sequences during consecutive repetition" $
      example $ do
        runMockT $ do
          consecutiveTimes 2 $ expect $ ReadFile "foo.txt" |-> "A" |-> "B"

          result <-
            (,,,) <$> readFile "foo.txt"
              <*> readFile "foo.txt"
              <*> readFile "foo.txt"
              <*> readFile "foo.txt"
          liftIO $ result `shouldBe` ("A", "B", "A", "B")

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
        ref <- newIORef ""
        runMockT $ do
          expectAny $
            WriteFile_ (eq "foo.txt") anything
              |=> \(WriteFile _ c) -> liftIO (writeIORef ref c)
          writeFile "foo.txt" "open sesame"
        readIORef ref `shouldReturn` "open sesame"

    it "respects expectations added by a response" $
      example $ do
        let setExpectations = do
              expectAny $
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

    it "has a correct implementation of MonadReader" $
      example $ do
        flip runReaderT "read me" $
          runMockT $ do
            expectAny $ ReadFile_ anything |=> const ask

            a <- readFile ""
            liftIO (a `shouldBe` "read me")

            local (++ " too") $ do
              b <- readFile ""
              liftIO (b `shouldBe` "read me too")

    it "has a correct implementation of MonadState" $
      example $ do
        filesRead <- flip execStateT (0 :: Int) $
          runMockT $ do
            expectAny $
              ReadFile_ anything
                |=> \_ -> modify (+ 1) >> return ""

            _ <- readFile "foo.txt"
            _ <- readFile "bar.txt"
            return ()

        filesRead `shouldBe` 2

    it "describes expectations when asked" $
      example . runMockT $ do
        expectAny $ ReadFile_ anything
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

    it "allows the user to override a default with setDefault" $
      example $
        runMockT $ do
          expectAny $ ReadFile_ anything

          r1 <- readFile "foo.txt"

          setDefault $ ReadFile "foo.txt" |-> "foo"
          r2 <- readFile "foo.txt"
          r3 <- readFile "bar.txt"

          liftIO (r1 `shouldBe` "")
          liftIO (r2 `shouldBe` "foo")
          liftIO (r3 `shouldBe` "")

    it "adopts lax behavior for onUnexpected" $
      example $
        runMockT $ do
          onUnexpected $ ReadFile "foo.txt" |-> "foo"
          r <- readFile "foo.txt"
          liftIO (r `shouldBe` "foo")

    it "prefers expect over onUnexpected" $
      example $
        runMockT $ do
          expectAny $ ReadFile_ anything |-> "bar"
          onUnexpected $ ReadFile "foo.txt" |-> "foo"
          r <- readFile "foo.txt"
          liftIO (r `shouldBe` "bar")

    it "doesn't adopt lax behavior for setDefault" $
      example $
        runMockT $ do
          onUnexpected $ ReadFile "foo.txt" |-> "foo"
          r <- readFile "foo.txt"
          liftIO (r `shouldBe` "foo")

    it "checks ambiguity when asked" $
      example $ do
        let setExpectations = do
              expect $ ReadFile_ anything
              expect $ ReadFile "foo.txt"
              setAmbiguityCheck True

            failure = runMockT $ do
              setExpectations
              _ <- readFile "foo.txt"
              _ <- readFile "bar.txt"
              return ()

            success = runMockT $ do
              setExpectations
              _ <- readFile "bar.txt"
              _ <- readFile "foo.txt"
              return ()

        success
        failure `shouldThrow` errorWith ("Ambiguous action" `isInfixOf`)

    describe "nestMockT" $ do
      it "checks nested context early" $ do
        example $ do
          let success = runMockT $ do
                expect $ WriteFile "final.txt" "final"
                nestMockT $ do
                  expect $ WriteFile "foo.txt" "foo"
                  writeFile "foo.txt" "foo"
                writeFile "final.txt" "final"

              failure = runMockT $ do
                nestMockT $ do
                  expect $ WriteFile "foo.txt" "foo"
                writeFile "foo.txt" "foo"

          success
          failure `shouldThrow` anyException

      it "updates the right context when nesting" $
        example $
          runMockT $ do
            expect $ ReadFile "foo.txt" |-> "foo #1" |-> "foo #2"
            result <- nestMockT $ do
              expect $ ReadFile "bar.txt" |-> "bar #1" |-> "bar #2"
              (,,,)
                <$> readFile "foo.txt"
                <*> readFile "bar.txt"
                <*> readFile "foo.txt"
                <*> readFile "bar.txt"
            liftIO (result `shouldBe` ("foo #1", "bar #1", "foo #2", "bar #2"))

      it "inherits defaults correctly" $
        example $
          runMockT $ do
            onUnexpected $ ReadFile "foo.txt" |-> "foo"
            onUnexpected $ ReadFile "bar.txt" |-> "bar"
            result <- nestMockT $ do
              onUnexpected $ ReadFile "foo.txt" |-> "foo #2"
              (,) <$> readFile "foo.txt" <*> readFile "bar.txt"

            liftIO (result `shouldBe` ("foo #2", "bar"))

            result2 <- readFile "foo.txt"
            liftIO (result2 `shouldBe` "foo")

errorWith :: (String -> Bool) -> SomeException -> Bool
errorWith p e = p (show e)

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
x <&&> y = (&&) <$> x <*> y
