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

-- | The capabilities most of the Bluefin tests share, made mockable with
-- 'makeMockableCapability', together with the code under test that uses
-- them.  None of the code under test knows HMock exists.
module Bluefin.Capabilities where

import Bluefin.Compound
  ( Generic,
    Handle,
    OneWayCoercible (..),
    OneWayCoercibleHandle (..),
    gOneWayCoercible,
    mapHandle,
  )
import Bluefin.Eff (Eff, type (<:))
import Data.Char (toUpper)
import Test.HMock.Bluefin

-- A Bluefin dynamic effect, exactly as a user would write it. ---------------

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

makeMockableCapability [t|FileSystem|]

-- A second, unrelated capability, to show one source serves many. ----------

data Clock e = MkClock
  { currentTime :: Eff e Int,
    sleep :: Int -> Eff e ()
  }
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle Clock

instance (e <: es) => OneWayCoercible (Clock e) (Clock es) where
  oneWayCoercibleImpl = gOneWayCoercible

currentTime' :: (e <: es) => Clock e -> Eff es Int
currentTime' clock = currentTime (mapHandle clock)

sleep' :: (e <: es) => Clock e -> Int -> Eff es ()
sleep' clock n = sleep (mapHandle clock) n

makeMockableCapability [t|Clock|]

-- A capability with its own type parameters. --------------------------------

data KVStore k v e = MkKVStore
  { getKey :: k -> Eff e (Maybe v),
    putKey :: k -> v -> Eff e ()
  }
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle (KVStore k v)

instance
  (e <: es) =>
  OneWayCoercible (KVStore k v e) (KVStore k v es)
  where
  oneWayCoercibleImpl = gOneWayCoercible

getKey' :: (e <: es) => KVStore k v e -> k -> Eff es (Maybe v)
getKey' kv k = getKey (mapHandle kv) k

putKey' :: (e <: es) => KVStore k v e -> k -> v -> Eff es ()
putKey' kv k v = putKey (mapHandle kv) k v

makeMockableCapability [t|KVStore String Int|]

-- Code under test.  None of this knows HMock exists. ------------------------

shout :: (e <: es) => FileSystem e -> FilePath -> FilePath -> Eff es String
shout fs from to = do
  contents <- readFile' fs from
  let loud = map toUpper contents
  writeFile' fs to loud
  return loud

backupWithTimestamp ::
  (e1 <: es, e2 <: es) =>
  FileSystem e1 ->
  Clock e2 ->
  FilePath ->
  Eff es FilePath
backupWithTimestamp fs clock path = do
  contents <- readFile' fs path
  t <- currentTime' clock
  let dest = path ++ "." ++ show t
  writeFile' fs dest contents
  return dest

cachedLookup ::
  (e1 <: es, e2 <: es) =>
  KVStore String Int e1 ->
  Clock e2 ->
  String ->
  Eff es Int
cachedLookup kv clock key = do
  cached <- getKey' kv key
  case cached of
    Just v -> return v
    Nothing -> do
      v <- currentTime' clock
      putKey' kv key v
      return v

archive ::
  (e1 <: es, e2 <: es) =>
  FileSystem e1 ->
  FileSystem e2 ->
  FilePath ->
  Eff es ()
archive src dst path = readFile' src path >>= writeFile' dst (path ++ ".bak")
