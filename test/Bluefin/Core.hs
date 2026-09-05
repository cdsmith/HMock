{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The runtime behaviour of Bluefin mocks: expectations and responses,
-- instances, source-wide rules, threads, combinators, checks, and nesting.
-- The capabilities come from "Bluefin.Capabilities".  What the splice
-- generates for each shape of declaration is tested in "Bluefin.TH".
module Bluefin.Core where

import Bluefin.Capabilities
import Bluefin.Eff (Eff, runEff, type (<:))
import Bluefin.IO (effIO, withEffToIO_)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (ErrorCall)
import Control.Monad (forM_, replicateM)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Test.HMock.Bluefin
import Test.Hspec
import Test.Predicates (anything, eq, gt)

basicSpec :: SpecWith ()
basicSpec = describe "Bluefin (basics)" $ do
  it "generates a working mock from a capability record" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      expect fs $ ReadFile "in.txt" |-> "lorem ipsum"
      expect fs $ WriteFile "out.txt" "LOREM IPSUM"
      shout (mocked fs) "in.txt" "out.txt"
    result `shouldBe` "LOREM IPSUM"

  it "generates matchers as well as actions" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      expectAny fs $ ReadFile_ anything |-> "x"
      expect fs $ WriteFile_ (eq "out.txt") anything
      shout (mocked fs) "whatever.txt" "out.txt"
    result `shouldBe` "X"

  it "serves several capabilities from one source" $ do
    dest <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      clock <- newMock source
      expect fs $ ReadFile "data.txt" |-> "payload"
      expect clock $ CurrentTime |-> 1234
      expect fs $ WriteFile "data.txt.1234" "payload"
      backupWithTimestamp (mocked fs) (mocked clock) "data.txt"
    dest `shouldBe` "data.txt.1234"

  it "mocks a capability with extra type parameters" $ do
    value <- runEff $ \io -> runMock io $ \source -> do
      kv <- newMock source
      clock <- newMock source
      expect kv $ GetKey "answer" |-> Nothing
      expect clock $ CurrentTime |-> 42
      expect kv $ PutKey "answer" 42
      cachedLookup (mocked kv) (mocked clock) "answer"
    value `shouldBe` 42

  it "mocks a nullary operation" $ do
    t <- runEff $ \io -> runMock io $ \source -> do
      clock <- newMock source
      expect clock $ CurrentTime |-> 7
      currentTime' (mocked clock)
    t `shouldBe` 7

  it "matches with predicates on arguments" $
    runEff $ \io -> runMock io $ \source -> do
      clock <- newMock source
      expect clock $ Sleep_ (gt 100)
      sleep' (mocked clock) 500

  it "reports the operation name and arguments on a mismatch" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          fs <- newMock source
          expect fs $ ReadFile "expected.txt" |-> "x"
          readFile' (mocked fs) "actual.txt"
    action `shouldThrow` \e ->
      let msg = show (e :: ErrorCall)
       in -- Named for the operation the caller wrote, not the record field
          -- that implements it.
          "readFile" `isInfixOf` msg
            && not ("Impl" `isInfixOf` msg)
            && "actual.txt" `isInfixOf` msg

  it "keeps an allowUnexpected response away from expected calls" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      allowUnexpected fs $ ReadFile_ anything |-> "from allowUnexpected"
      byDefault fs $ ReadFile_ anything |-> "from byDefault"
      expect fs $ ReadFile "in.txt"
      readFile' (mocked fs) "in.txt"
    result `shouldBe` "from byDefault"

  it "prefers an allowUnexpected response to byDefault for an unexpected call" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      allowUnexpected fs $ ReadFile_ anything |-> "from allowUnexpected"
      byDefault fs $ ReadFile_ anything |-> "from byDefault"
      readFile' (mocked fs) "in.txt"
    result `shouldBe` "from allowUnexpected"

  it "supports byDefault and allowUnexpected" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      allowUnexpected fs $ WriteFile_ anything anything
      byDefault fs $ ReadFile_ anything |-> "default"
      expectAny fs $ ReadFile_ anything
      shout (mocked fs) "anything.txt" "out.txt"
    result `shouldBe` "DEFAULT"

  it "runs responses in the surrounding Eff scope" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      expect fs $
        ReadFile "in.txt" |=> \(ReadFile path) -> do
          effIO io (putStrLn ("mock read " ++ path))
          return "from a response"
      readFile' (mocked fs) "in.txt"
    result `shouldBe` "from a response"

  it "lets a response add further expectations" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      expect fs $
        ReadFile "index.txt" |=> \_ -> do
          -- The response uses the mock handle from the enclosing scope.
          expect fs $ ReadFile "page.txt" |-> "page contents"
          return "page.txt"
      next <- readFile' (mocked fs) "index.txt"
      readFile' (mocked fs) next
    result `shouldBe` "page contents"

  it "sequences expectations across capabilities" $
    runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      clock <- newMock source
      inSequence
        [ expect clock $ CurrentTime |-> 1,
          expect fs $ ReadFile "in.txt" |-> "x"
        ]
      _ <- currentTime' (mocked clock)
      _ <- readFile' (mocked fs) "in.txt"
      return ()

  it "fails when a cross-capability sequence is violated" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          fs <- newMock source
          clock <- newMock source
          inSequence
            [ expect clock $ CurrentTime |-> 1,
              expect fs $ ReadFile "in.txt" |-> "x"
            ]
          _ <- readFile' (mocked fs) "in.txt"
          currentTime' (mocked clock)
    action `shouldThrow` anyErrorCall

-- Two instances of the same capability are not interchangeable. -------------

-- | 'mockName' at a fixed capability, so that the instance type is determined.
fsName :: Mock root resp FileSystem e -> String
fsName = mockName

instancesSpec :: SpecWith ()
instancesSpec = describe "Bluefin (capability instances)" $ do
  it "keeps two instances of one capability apart" $
    runEff $ \io -> runMock io $ \source -> do
      src <- newMock source
      dst <- newMock source
      expect src $ ReadFile "a.txt" |-> "contents"
      expect dst $ WriteFile "a.txt.bak" "contents"
      archive (mocked src) (mocked dst) "a.txt"

  it "rejects a call made through the wrong instance" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          src <- newMock source
          dst <- newMock source
          expect src $ ReadFile "a.txt" |-> "contents"
          expect dst $ WriteFile "a.txt.bak" "contents"
          -- Reads through dst, which expects no read of its own.
          archive (mocked dst) (mocked dst) "a.txt"
    action `shouldThrow` anyErrorCall

  it "names the offending instance in the failure message" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          src <- newMock source
          dst <- newMock source
          expect src $ ReadFile "a.txt" |-> "contents"
          expect dst $ ReadFile "b.txt" |-> "contents"
          _ <- readFile' (mocked src) "a.txt"
          readFile' (mocked dst) "a.txt"
    action `shouldThrow` \e ->
      "on instance #2" `isInfixOf` show (e :: ErrorCall)

  it "names instances on request, and keeps the names unique" $ do
    names <- runEff $ \io -> runMock io $ \source -> do
      a <- newNamedMock "fs" source
      b <- newNamedMock "fs" source
      c <- newNamedMock "log" source
      d <- newNamedMock "fs" source
      e <- newMock source
      return (map fsName [a, b, c, d, e])
    names `shouldBe` ["fs", "fs_2", "log", "fs_3", "#5"]

  it "keeps an unnamed instance's name unique too" $ do
    names <- runEff $ \io -> runMock io $ \source -> do
      a <- newNamedMock "#2" source
      b <- newMock source
      return (map fsName [a, b])
    names `shouldBe` ["#2", "#2_2"]

  it "reports a call through the wrong instance as a near miss" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          src <- newNamedMock "src" source
          dst <- newNamedMock "dst" source
          expect src $ ReadFile "a.txt" |-> "contents"
          expect dst $ WriteFile "a.txt.bak" "contents"
          archive (mocked dst) (mocked dst) "a.txt"
    action `shouldThrow` \e ->
      let msg = show (e :: ErrorCall)
       in "Wrong instance: readFile \"a.txt\" on instance dst" `isInfixOf` msg
            && "Expected on instance src, but called on instance dst" `isInfixOf` msg

  it "keeps per-instance defaults apart" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      quiet <- newMock source
      loud <- newMock source
      allowUnexpected quiet $ ReadFile_ anything |-> "shh"
      allowUnexpected loud $ ReadFile_ anything |-> "HEY"
      a <- readFile' (mocked quiet) "x"
      b <- readFile' (mocked loud) "x"
      return (a, b)
    result `shouldBe` ("shh", "HEY")

-- Source-wide rules. ---------------------------------------------------------

-- | A t'MockSource' as the target means every instance of the capability the
-- rule mentions, so these exercise the engine's wildcard match.
globalSpec :: SpecWith ()
globalSpec = describe "Bluefin (source-wide rules)" $ do
  it "matches a call through any instance" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      a <- newMock source
      b <- newMock source
      expectN 2 source $ ReadFile "shared.txt" |-> "same"
      (,) <$> readFile' (mocked a) "shared.txt" <*> readFile' (mocked b) "shared.txt"
    result `shouldBe` ("same", "same")

  it "matches a call through an instance created after the rule" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      expect source $ ReadFile "later.txt" |-> "from before"
      fs <- newMock source
      readFile' (mocked fs) "later.txt"
    result `shouldBe` "from before"

  it "leaves instance-specific rules specific" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          a <- newMock source
          b <- newMock source
          expect source $ ReadFile "any.txt" |-> "any"
          expect b $ ReadFile "only-b.txt" |-> "b"
          _ <- readFile' (mocked b) "any.txt"
          readFile' (mocked a) "only-b.txt"
    action `shouldThrow` \e ->
      "Wrong instance" `isInfixOf` show (e :: ErrorCall)

  it "applies source-wide defaults to every instance" $ do
    runEff $ \io -> runMock io $ \source -> do
      a <- newMock source
      b <- newMock source
      allowUnexpected source $ WriteFile_ anything anything
      writeFile' (mocked a) "a.txt" "x"
      writeFile' (mocked b) "b.txt" "y"

-- Threads. ------------------------------------------------------------------

-- | Mocks share one expectation set across threads.  'withEffToIO_' is
-- Bluefin's 'withRunInIO', so this is the same shape as 'withMockT'.
threadSpec :: SpecWith ()
threadSpec = describe "Bluefin (threads)" $ do
  it "shares expectations across threads, in both directions" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      expect fs $ ReadFile "a.txt" |-> "from main"
      done <- effIO io newEmptyMVar
      withEffToIO_ io $ \unlift -> do
        _ <- forkIO $ unlift $ do
          expect fs $ ReadFile "b.txt" |-> "from thread"
          r <- readFile' (mocked fs) "a.txt"
          effIO io (putMVar done r)
        return ()
      fromThread <- effIO io (takeMVar done)
      fromMain <- readFile' (mocked fs) "b.txt"
      return (fromThread, fromMain)
    result `shouldBe` ("from main", "from thread")

  it "matches concurrent calls against one plan" $ do
    total <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      expectN 8 fs $ ReadFile_ anything |-> "x"
      results <- effIO io $ replicateM 8 newEmptyMVar
      withEffToIO_ io $ \unlift ->
        forM_ results $ \var -> forkIO $ unlift $ do
          r <- readFile' (mocked fs) "shared.txt"
          effIO io (putMVar var (length r))
      sum <$> effIO io (mapM takeMVar results)
    total `shouldBe` 8

-- Combinators. --------------------------------------------------------------

-- | The compound combinators build an 'Expectation' and install it later, so
-- these exercise a path the flat @expect@ never touches.
combinatorSpec :: SpecWith ()
combinatorSpec = describe "Bluefin (combinators)" $ do
  it "implements unordered expectations" $ do
    let plan fs clock = inAnyOrder
          [ expect fs $ WriteFile "a.txt" "a",
            expect clock $ CurrentTime |-> 1
          ]
    -- Either order satisfies it.
    runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      clock <- newMock source
      plan fs clock
      writeFile' (mocked fs) "a.txt" "a"
      _ <- currentTime' (mocked clock)
      return ()
    runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      clock <- newMock source
      plan fs clock
      _ <- currentTime' (mocked clock)
      writeFile' (mocked fs) "a.txt" "a"

  it "fails when only part of an unordered group happens" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          fs <- newMock source
          clock <- newMock source
          inAnyOrder
            [ expect fs $ WriteFile "a.txt" "a",
              expect clock $ CurrentTime |-> 1
            ]
          writeFile' (mocked fs) "a.txt" "a"
    action `shouldThrow` anyErrorCall

  it "treats an empty sequence or group as no expectation" $
    runEff $ \io -> runMock io $ \_source -> do
      inSequence []
      inAnyOrder []

  it "rejects an empty choice immediately" $ do
    let action = runEff $ \io -> runMock io $ \_source -> anyOf []
    action `shouldThrow` \e ->
      "anyOf requires at least one" `isInfixOf` show (e :: ErrorCall)

  it "implements choice" $ do
    -- Exactly one of the alternatives must happen.
    runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      anyOf
        [ expect fs $ WriteFile "status.txt" "go",
          expect fs $ WriteFile "status.txt" "stop"
        ]
      writeFile' (mocked fs) "status.txt" "stop"

  it "fails when no alternative happens" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          fs <- newMock source
          anyOf
            [ expect fs $ WriteFile "status.txt" "go",
              expect fs $ WriteFile "status.txt" "stop"
            ]
    action `shouldThrow` anyErrorCall

  it "implements interleaved repetition" $ do
    runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      times (atLeast 2) $
        inAnyOrder
          [ expect fs $ WriteFile "foo.txt" "a",
            expect fs $ WriteFile "bar.txt" "b"
          ]
      let h = mocked fs
      writeFile' h "foo.txt" "a"
      writeFile' h "bar.txt" "b"
      writeFile' h "bar.txt" "b"
      writeFile' h "foo.txt" "a"

  it "fails repetition that happens too few times" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          fs <- newMock source
          times (atLeast 2) $ expect fs $ WriteFile "foo.txt" "a"
          writeFile' (mocked fs) "foo.txt" "a"
    action `shouldThrow` anyErrorCall

  it "implements consecutive repetition" $ do
    runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      consecutiveTimes (atLeast 2) $
        inSequence
          [ expect fs $ WriteFile "foo.txt" "a",
            expect fs $ WriteFile "bar.txt" "b"
          ]
      let h = mocked fs
      writeFile' h "foo.txt" "a"
      writeFile' h "bar.txt" "b"
      writeFile' h "foo.txt" "a"
      writeFile' h "bar.txt" "b"

  it "rejects interleaving under consecutive repetition" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          fs <- newMock source
          consecutiveTimes (atLeast 2) $
            inSequence
              [ expect fs $ WriteFile "foo.txt" "a",
                expect fs $ WriteFile "bar.txt" "b"
              ]
          let h = mocked fs
          writeFile' h "foo.txt" "a"
          writeFile' h "foo.txt" "a"
          writeFile' h "bar.txt" "b"
          writeFile' h "bar.txt" "b"
    action `shouldThrow` anyErrorCall

  it "repeats response sequences during repetition" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      times 2 $ expect fs $ ReadFile "foo.txt" |-> "A" |-> "B"
      let h = mocked fs
      (,,,) <$> readFile' h "foo.txt"
        <*> readFile' h "foo.txt"
        <*> readFile' h "foo.txt"
        <*> readFile' h "foo.txt"
    result `shouldBe` ("A", "B", "A", "B")

  it "counts an exact multiplicity" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          clock <- newMock source
          expectN 2 clock $ CurrentTime |-> 1
          _ <- currentTime' (mocked clock)
          _ <- currentTime' (mocked clock)
          currentTime' (mocked clock)
    action `shouldThrow` anyErrorCall

-- Side effects, severities, and inspection. ---------------------------------

configSpec :: SpecWith ()
configSpec = describe "Bluefin (defaults and checks)" $ do
  it "runs a side effect on every matching call" $ do
    calls <- newIORef (0 :: Int)
    result <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      whenever fs $
        ReadFile_ anything |=> \_ -> do
          effIO io (modifyIORef' calls (+ 1))
          return "ignored"
      expectAny fs $ ReadFile_ anything |-> "real"
      let h = mocked fs
      (,) <$> readFile' h "a.txt" <*> readFile' h "b.txt"
    result `shouldBe` ("real", "real")
    readIORef calls `shouldReturn` 2

  it "can downgrade unexpected actions to a default" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      setUninterestingActionCheck source Ignore
      setUnexpectedActionCheck source Ignore
      fs <- newMock source
      readFile' (mocked fs) "never-expected.txt"
    result `shouldBe` ""

  it "can downgrade unmet expectations" $
    runEff $ \io -> runMock io $ \source -> do
      setUnmetExpectationCheck source Ignore
      fs <- newMock source
      expect fs $ ReadFile "never-read.txt" |-> "x"

  it "can make ambiguity an error" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          setAmbiguityCheck source Error
          fs <- newMock source
          expect fs $ ReadFile "a.txt" |-> "one"
          expect fs $ ReadFile_ anything |-> "two"
          readFile' (mocked fs) "a.txt"
    action `shouldThrow` \e ->
      "Ambiguous action" `isInfixOf` show (e :: ErrorCall)

  it "describes outstanding expectations" $ do
    described <- runEff $ \io -> runMock io $ \source -> do
      setUnmetExpectationCheck source Ignore
      fs <- newMock source
      expect fs $ ReadFile "pending.txt" |-> "x"
      describeExpectations source
    described `shouldSatisfy` \d ->
      "readFile" `isInfixOf` d && "pending.txt" `isInfixOf` d

  it "verifies expectations early" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          fs <- newMock source
          expect fs $ ReadFile "pending.txt" |-> "x"
          verifyExpectations source
    action `shouldThrow` \e ->
      "Unmet expectations" `isInfixOf` show (e :: ErrorCall)

  it "verifies cleanly once expectations are met" $
    runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      expect fs $ ReadFile "a.txt" |-> "x"
      _ <- readFile' (mocked fs) "a.txt"
      verifyExpectations source

-- Nested expectation layers. -------------------------------------------------

nestingSpec :: SpecWith ()
nestingSpec = describe "Bluefin (nested expectations)" $ do
  it "checks a nested layer at the end of its block" $
    runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      expect fs $ WriteFile "final.txt" "final"
      nestMockSource source $ \nested -> do
        fs' <- adoptMock nested fs
        expect fs' $ WriteFile "foo.txt" "foo"
        writeFile' (mocked fs') "foo.txt" "foo"
      writeFile' (mocked fs) "final.txt" "final"

  it "fails when a nested expectation outlives its block" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          fs <- newMock source
          nestMockSource source $ \nested -> do
            fs' <- adoptMock nested fs
            expect fs' $ WriteFile "foo.txt" "foo"
          -- Satisfied only after the block ended, which is too late.
          writeFile' (mocked fs) "foo.txt" "foo"
    action `shouldThrow` anyErrorCall

  it "keeps outer expectations visible inside the block" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      expect fs $ ReadFile "foo.txt" |-> "foo #1" |-> "foo #2"
      nestMockSource source $ \nested -> do
        fs' <- adoptMock nested fs
        expect fs' $ ReadFile "bar.txt" |-> "bar #1" |-> "bar #2"
        let h = mocked fs'
        (,,,) <$> readFile' h "foo.txt"
          <*> readFile' h "bar.txt"
          <*> readFile' h "foo.txt"
          <*> readFile' h "bar.txt"
    result `shouldBe` ("foo #1", "bar #1", "foo #2", "bar #2")

  it "serves several adopted mocks from one nested layer" $
    runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      clock <- newMock source
      nestMockSource source $ \nested -> do
        fs' <- adoptMock nested fs
        clock' <- adoptMock nested clock
        inSequence
          [ expect clock' $ CurrentTime |-> 1234,
            expect fs' $ WriteFile "log.1234" "entry"
          ]
        _ <- currentTime' (mocked clock')
        writeFile' (mocked fs') "log.1234" "entry"

  it "nests a source inside a nested source" $
    runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      nestMockSource source $ \outer -> do
        fsOuter <- adoptMock outer fs
        expect fsOuter $ WriteFile "outer.txt" "o"
        nestMockSource outer $ \inner -> do
          fsInner <- adoptMock inner fsOuter
          expect fsInner $ WriteFile "inner.txt" "i"
          writeFile' (mocked fsInner) "inner.txt" "i"
        writeFile' (mocked fsOuter) "outer.txt" "o"

  it "adopts a mock created in a nested block into the enclosing layer" $
    runEff $ \io -> runMock io $ \source -> do
      fs <- adoptedOutward source
      writeFile' (mocked fs) "outer.txt" "o"

-- | Creates a mock in a nested block and adopts it into the enclosing source,
-- with an expectation on each.  The outer one may be met after the block.
--
-- The signature matters: a mock returned from 'nestMockSource' has a scope
-- that GHC only learns from the block's result, and a use of it before that
-- is resolved through Bluefin's incoherent '<:' instance the wrong way.
adoptedOutward ::
  (e <: es) => MockSource root resp e -> Eff es (Mock root resp FileSystem e)
adoptedOutward source = nestMockSource source $ \nested -> do
  inner <- newMock nested
  expect inner $ WriteFile "inner.txt" "i"
  outer <- adoptMock source inner
  expect outer $ WriteFile "outer.txt" "o"
  writeFile' (mocked inner) "inner.txt" "i"
  return outer

-- All specs. ------------------------------------------------------------------

-- | Every spec in this module.
bluefinCoreSpec :: SpecWith ()
bluefinCoreSpec = do
  basicSpec
  instancesSpec
  globalSpec
  threadSpec
  combinatorSpec
  configSpec
  nestingSpec
