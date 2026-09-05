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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A hand-written Bluefin mock, spelling out everything that
-- 'Test.HMock.Bluefin.TH.makeMockableCapability' generates.  This exists to
-- pin down the shape of the generated code, and to keep the runtime side
-- honest independently of the Template Haskell.
module Bluefin.Manual where

import Bluefin.Compound
  ( Generic,
    Handle,
    OneWayCoercible (..),
    OneWayCoercibleHandle (..),
    gOneWayCoercible,
    mapHandle,
  )
import Bluefin.Eff (Eff, runEff, type (<:))
import Bluefin.IO (effIO)
import Control.Exception (ErrorCall)
import Data.Char (toUpper)
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Test.HMock.Bluefin
import Test.Hspec
import Test.Predicates (Predicate (..), anything, eq)

-- | A Bluefin dynamic effect, in the style of "Bluefin.Compound".  This is
-- ordinary user code, with no HMock involvement.
data FileSystem e = MkFileSystem
  { readFileImpl :: FilePath -> Eff e String,
    writeFileImpl :: FilePath -> String -> Eff e ()
  }
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle FileSystem

instance (e <: es) => OneWayCoercible (FileSystem e) (FileSystem es) where
  oneWayCoercibleImpl = gOneWayCoercible

readFile' :: (e <: es) => FileSystem e -> FilePath -> Eff es String
readFile' fs path = readFileImpl (mapHandle fs) path

writeFile' :: (e <: es) => FileSystem e -> FilePath -> String -> Eff es ()
writeFile' fs path contents = writeFileImpl (mapHandle fs) path contents

-- Everything below is what makeMockableCapability generates. ---------------

instance MockableBase FileSystem where
  data Action FileSystem name a where
    ReadFile :: FilePath -> Action FileSystem "readFile" String
    WriteFile ::
      FilePath -> String -> Action FileSystem "writeFile" ()

  data Matcher FileSystem name a where
    ReadFile_ ::
      Predicate FilePath -> Matcher FileSystem "readFile" String
    WriteFile_ ::
      Predicate FilePath ->
      Predicate String ->
      Matcher FileSystem "writeFile" ()

  showAction (ReadFile path) = unwords ["readFile", show path]
  showAction (WriteFile path contents) =
    unwords ["writeFile", show path, show contents]

  showMatcher _ (ReadFile_ p) = unwords ["readFile", "«" ++ show p ++ "»"]
  showMatcher _ (WriteFile_ p q) =
    unwords ["writeFile", "«" ++ show p ++ "»", "«" ++ show q ++ "»"]

  matchAction (ReadFile_ p) (ReadFile path)
    | null mismatches = Match
    | otherwise = NoMatch mismatches
    where
      mismatches = catMaybes (zipWith (fmap . (,)) [1 ..] [check p path])
  matchAction (WriteFile_ p q) (WriteFile path contents)
    | null mismatches = Match
    | otherwise = NoMatch mismatches
    where
      mismatches =
        catMaybes
          (zipWith (fmap . (,)) [1 ..] [check p path, check q contents])

check :: Predicate a -> a -> Maybe String
check p x = if accept p x then Nothing else Just (explain p x)

instance ExactMatch FileSystem "readFile" String where
  exactMatcher (ReadFile path) = ReadFile_ (eq path)

instance ExactMatch FileSystem "writeFile" () where
  exactMatcher (WriteFile path contents) = WriteFile_ (eq path) (eq contents)

instance MockableCapability FileSystem where
  mocked cap =
    MkFileSystem
      { readFileImpl = \path -> mockMethod cap (ReadFile path),
        writeFileImpl = \path contents ->
          mockMethod cap (WriteFile path contents)
      }

-- Tests. -------------------------------------------------------------------

-- | The code under test: ordinary Bluefin code that knows nothing of HMock.
shout :: (e <: es) => FileSystem e -> FilePath -> FilePath -> Eff es String
shout fs from to = do
  contents <- readFile' fs from
  let loud = map toUpper contents
  writeFile' fs to loud
  return loud

-- | Code that reads from one file system and writes to another.
copyBetween ::
  (e1 <: es, e2 <: es) =>
  FileSystem e1 ->
  FileSystem e2 ->
  FilePath ->
  FilePath ->
  Eff es ()
copyBetween src dst from to = readFile' src from >>= writeFile' dst to

bluefinManualSpec :: SpecWith ()
bluefinManualSpec = describe "Bluefin (hand-written)" $ do
  it "matches expectations and returns responses" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      expect fs $ ReadFile "in.txt" |-> "lorem ipsum"
      expect fs $ WriteFile "out.txt" "LOREM IPSUM"
      shout (mocked fs) "in.txt" "out.txt"
    result `shouldBe` "LOREM IPSUM"

  it "fails when an expectation is unmet" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          fs <- newMock source
          expect fs $ ReadFile "in.txt" |-> "lorem ipsum"
          expect fs $ WriteFile "never.txt" "unused"
          readFile' (mocked fs) "in.txt"
    action `shouldThrow` anyErrorCall

  it "fails on an unexpected action" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          fs <- newMock source
          expect fs $ ReadFile "in.txt" |-> "lorem ipsum"
          _ <- readFile' (mocked fs) "in.txt"
          writeFile' (mocked fs) "out.txt" "surprise"
    action `shouldThrow` anyErrorCall

  it "supports matchers and multiplicities" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      expectAny fs $ ReadFile_ anything |-> "x"
      expectN (atLeast 2) fs $ WriteFile_ anything anything
      let h = mocked fs
      a <- readFile' h "one.txt"
      b <- readFile' h "two.txt"
      writeFile' h "out.txt" a
      writeFile' h "out.txt" b
      return (a ++ b)
    result `shouldBe` "xx"

  it "runs responses that use the surrounding Eff scope" $ do
    logged <- runEff $ \io -> runMock io $ \source -> do
      fs <- newMock source
      expect fs $
        ReadFile "in.txt" |=> \(ReadFile path) -> do
          effIO io (putStrLn ("mock read " ++ path))
          return "from a response"
      readFile' (mocked fs) "in.txt"
    logged `shouldBe` "from a response"

  describe "capability instances" $ do
    it "keeps two instances of one capability apart" $
      runEff $ \io -> runMock io $ \source -> do
        src <- newMock source
        dst <- newMock source
        expect src $ ReadFile "a.txt" |-> "contents"
        expect dst $ WriteFile "b.txt" "contents"
        copyBetween (mocked src) (mocked dst) "a.txt" "b.txt"

    it "rejects a call made through the wrong instance" $ do
      let action = runEff $ \io -> runMock io $ \source -> do
            src <- newMock source
            dst <- newMock source
            expect src $ ReadFile "a.txt" |-> "contents"
            expect dst $ WriteFile "b.txt" "contents"
            -- Reads through dst, which expects no read of its own.
            copyBetween (mocked dst) (mocked dst) "a.txt" "b.txt"
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
