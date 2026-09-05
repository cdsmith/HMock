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

-- | The Bluefin quick start from README.md, verbatim, so that it can't rot.
module Bluefin.Readme where

import Bluefin.Compound
  ( Generic,
    Handle,
    OneWayCoercible (..),
    OneWayCoercibleHandle (..),
    gOneWayCoercible,
    mapHandle,
  )
import Bluefin.Eff (Eff, runEff, type (<:))
import Test.HMock.Bluefin
import Test.Hspec
import Prelude hiding (readFile, writeFile)

data FileSystem e = MkFileSystem
  { readFileImpl :: FilePath -> Eff e String,
    writeFileImpl :: FilePath -> String -> Eff e ()
  }
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle FileSystem

instance (e <: es) => OneWayCoercible (FileSystem e) (FileSystem es) where
  oneWayCoercibleImpl = gOneWayCoercible

readFile :: (e <: es) => FileSystem e -> FilePath -> Eff es String
readFile fs path = readFileImpl (mapHandle fs) path

writeFile :: (e <: es) => FileSystem e -> FilePath -> String -> Eff es ()
writeFile fs path contents = writeFileImpl (mapHandle fs) path contents

copyFile :: (e <: es) => FileSystem e -> FilePath -> FilePath -> Eff es ()
copyFile fs a b = readFile fs a >>= writeFile fs b

makeMockableCapability [t|FileSystem|]

test_copyFile :: IO ()
test_copyFile = runEff $ \io -> runMock io $ \source -> do
  fs <- newMock source

  expect fs $ ReadFile "foo.txt" |-> "contents"
  expect fs $ WriteFile "bar.txt" "contents"

  copyFile (mocked fs) "foo.txt" "bar.txt"

-- | Not in the README, but the natural next step from it: expectations belong
-- to an instance, so two mocks of one capability are told apart.
archive ::
  (e1 <: es, e2 <: es) =>
  FileSystem e1 ->
  FileSystem e2 ->
  FilePath ->
  Eff es ()
archive src dst path = readFile src path >>= writeFile dst (path ++ ".bak")

test_archive :: IO ()
test_archive = runEff $ \io -> runMock io $ \source -> do
  src <- newMock source
  dst <- newMock source

  expect src $ ReadFile "a.txt" |-> "contents"
  expect dst $ WriteFile "a.txt.bak" "contents"

  archive (mocked src) (mocked dst) "a.txt"

bluefinReadmeSpec :: SpecWith ()
bluefinReadmeSpec = describe "Bluefin (README quick start)" $ do
  it "works as documented" test_copyFile
  it "keeps two instances of one capability apart" test_archive
