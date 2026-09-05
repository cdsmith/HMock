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

-- | The shapes of capability declaration 'makeMockableCapability' accepts,
-- and what it generates for each: partly mockable records, field naming,
-- GADT record syntax, callback arguments, results without a 'Default', and
-- polymorphic results.  The declarations it rejects are tested in
-- "Bluefin.Splice", and the behaviour of the generated mocks in
-- "Bluefin.Core".
module Bluefin.TH where

import Bluefin.Compound
  ( Generic,
    Handle,
    OneWayCoercible (..),
    OneWayCoercibleHandle (..),
    gOneWayCoercible,
    makeOp,
    mapHandle,
    oneWayCoercibleTrustMe,
    useImpl,
    useImplUnder,
  )
import Bluefin.Eff (Eff, runEff, type (:&), type (<:))
import Bluefin.IO (effIO)
import Control.Exception (evaluate)
import Data.Default (def)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Typeable (Typeable)
import Test.HMock.Bluefin
import Test.Hspec
import Test.Predicates (anything, eq)

-- A capability HMock can only partly mock. ---------------------------------

-- | @withScope@ takes an effectful operation as an argument, in the style of
-- @Counter7@ and @DynamicReader@ in "Bluefin.Compound", so it can't become an
-- 'Action': its result isn't in the capability's own effect scope.  The
-- splice skips it, and we supply that one field by hand.
data Mixed e = MkMixed
  { plainOp :: Int -> Eff e String,
    withScope :: forall e' a. Eff e' a -> Eff (e' :& e) a
  }
  deriving (Handle) via OneWayCoercibleHandle Mixed

instance (e <: es) => OneWayCoercible (Mixed e) (Mixed es) where
  oneWayCoercibleImpl = oneWayCoercibleTrustMe $ \h ->
    MkMixed
      { plainOp = \n -> useImpl (plainOp h n),
        withScope = \k -> useImplUnder (withScope h k)
      }

plainOp' :: (e <: es) => Mixed e -> Int -> Eff es String
plainOp' cap n = plainOp (mapHandle cap) n

withScope' :: (e <: es) => Mixed e -> Eff es a -> Eff es a
withScope' cap k = makeOp (withScope (mapHandle cap) k)

makeMockableCapabilityWithOptions
  [t|Mixed|]
  def {capDeriveInstance = False}

instance MockableCapability Mixed where
  mocked cap =
    MkMixed
      { plainOp = \n -> mockMethod cap (PlainOp n),
        withScope = useImpl
      }

mixedSpec :: SpecWith ()
mixedSpec = describe "Bluefin (partly mockable capability)" $
  it "mocks the operations it can and leaves the rest to you" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      cap <- newMock source
      expect cap $ PlainOp 3 |-> "three"
      let h = mocked cap
      withScope' h (plainOp' h 3)
    result `shouldBe` "three"

-- Field naming. -------------------------------------------------------------

-- | A capability whose fields don't follow the @Impl@ convention: nothing is
-- stripped, so the operation keeps its own name.
data Plain e = MkPlain {ping :: Int -> Eff e Int}
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle Plain

instance (e <: es) => OneWayCoercible (Plain e) (Plain es) where
  oneWayCoercibleImpl = gOneWayCoercible

ping' :: (e <: es) => Plain e -> Int -> Eff es Int
ping' p n = ping (mapHandle p) n

makeMockableCapability [t|Plain|]

-- | The same capability with the convention, and stripping switched off.
data Kept e = MkKept {tickImpl :: Eff e Int}
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle Kept

instance (e <: es) => OneWayCoercible (Kept e) (Kept es) where
  oneWayCoercibleImpl = gOneWayCoercible

tick' :: (e <: es) => Kept e -> Eff es Int
tick' k = tickImpl (mapHandle k)

makeMockableCapabilityWithOptions [t|Kept|] def {capStripSuffix = ""}

namingSpec :: SpecWith ()
namingSpec = describe "Bluefin (operation naming)" $ do
  it "leaves a field alone when it has no Impl suffix" $ do
    n <- runEff $ \io -> runMock io $ \source -> do
      p <- newMock source
      expect p $ Ping 1 |-> 2
      ping' (mocked p) 1
    n `shouldBe` 2

  it "keeps the suffix when stripping is switched off" $ do
    n <- runEff $ \io -> runMock io $ \source -> do
      k <- newMock source
      expect k $ TickImpl |-> 5
      tick' (mocked k)
    n `shouldBe` 5

-- GADT record syntax. --------------------------------------------------------

-- | A capability declared in GADT record syntax, which reifies differently
-- from an ordinary record.
newtype Ticker e where
  MkTicker :: {nextTickImpl :: Eff e Int} -> Ticker e

makeMockableCapability [t|Ticker|]

gadtSpec :: SpecWith ()
gadtSpec = describe "Bluefin (GADT record syntax)" $
  it "mocks a capability declared in GADT record syntax" $ do
    n <- runEff $ \io -> runMock io $ \source -> do
      t <- newMock source
      expect t $ NextTick |-> 9
      nextTickImpl (mocked (mapHandle t))
    n `shouldBe` 9

-- Callbacks. ----------------------------------------------------------------

-- | A capability with callback arguments.  Such a record can't derive
-- 'Handle' generically, since callbacks are contravariant in the scope, so
-- these tests widen the t'Mock' instead and use the record's fields directly.
data Resource e = MkResource
  { withResourceImpl :: String -> (Int -> Eff e String) -> Eff e String,
    finallyImpl :: Eff e () -> Eff e Int
  }

makeMockableCapability [t|Resource|]

-- | Callback arguments reach the engine as IO functions, and a response runs
-- them with effIO.
callbackSpec :: SpecWith ()
callbackSpec = describe "Bluefin (callback arguments)" $ do
  it "lets a response invoke a callback" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      r <- newMock source
      expect r $
        WithResource_ (eq "db") anything |=> \(WithResource _ k) -> effIO io (k 7)
      withResourceImpl (mocked (mapHandle r)) "db" (\n -> return (show (n * 6)))
    result `shouldBe` "42"

  it "lets a callback call other mocks" $ do
    result <- runEff $ \io -> runMock io $ \source -> do
      r <- newMock source
      k <- newMock source
      expect k $ TickImpl |-> 10
      expect r $
        WithResource_ (eq "db") anything |=> \(WithResource _ f) -> effIO io (f 7)
      withResourceImpl (mocked (mapHandle r)) "db" $ \n -> do
        t <- tick' (mocked k)
        return (show (n + t))
    result `shouldBe` "17"

  it "unlifts a bare Eff argument" $ do
    ref <- newIORef (0 :: Int)
    n <- runEff $ \io -> runMock io $ \source -> do
      r <- newMock source
      expect r $ Finally_ anything |=> \(Finally act) -> effIO io act >> return 3
      finallyImpl (mocked (mapHandle r)) (effIO io (modifyIORef' ref (+ 1)))
    n `shouldBe` 3
    readIORef ref `shouldReturn` 1

-- A return type with no Default instance. ------------------------------------

-- | 'Token' has no 'Data.Default.Default' instance, so the splice must
-- delegate through 'mockDefaultlessMethod'.  Choosing 'mockMethod' here would
-- not compile.
newtype Token = Token String deriving (Eq, Show)

data Auth e = MkAuth {login :: String -> Eff e Token}
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle Auth

instance (e <: es) => OneWayCoercible (Auth e) (Auth es) where
  oneWayCoercibleImpl = gOneWayCoercible

login' :: (e <: es) => Auth e -> String -> Eff es Token
login' auth user = login (mapHandle auth) user

makeMockableCapability [t|Auth|]

defaultlessSpec :: SpecWith ()
defaultlessSpec = describe "Bluefin (return type without Default)" $ do
  it "delegates through mockDefaultlessMethod" $ do
    t <- runEff $ \io -> runMock io $ \source -> do
      auth <- newMock source
      expect auth $ Login "alice" |-> Token "abc"
      login' (mocked auth) "alice"
    t `shouldBe` Token "abc"

  it "returns undefined when an expectation has no response" $ do
    let action = runEff $ \io -> runMock io $ \source -> do
          auth <- newMock source
          expect auth $ Login "alice"
          Token s <- login' (mocked auth) "alice"
          -- Forcing the surrogate is what fails, not the call itself.
          effIO io (evaluate (length s))
    action `shouldThrow` anyErrorCall

-- Polymorphic results. --------------------------------------------------------

-- | The engine matches on the result's 'TypeRep', so an operation whose result
-- type is a variable can be mocked only if the variable is 'Typeable'.  The
-- splice must generate a mock for @polyResultImpl@ and skip
-- @nonTypeableImpl@, rather than emit code that doesn't typecheck.  Like
-- t'Resource' above, the record can't derive 'Handle' generically, so the tests
-- widen the t'Mock' instead.
data PolyResult e = MkPolyResult
  { polyResultImpl :: forall a. Typeable a => a -> Eff e a,
    nonTypeableImpl :: forall a. a -> Eff e a
  }

makeMockableCapabilityWithOptions
  [t|PolyResult|]
  def {capDeriveInstance = False}

instance MockableCapability PolyResult where
  mocked cap =
    MkPolyResult
      { polyResultImpl = \x -> mockDefaultlessMethod cap (PolyResult x),
        nonTypeableImpl = return
      }

-- | The same shape as 'PolyResult', with the splice generating the instance:
-- the field's own @Typeable a@ context has to reach the delegating call in
-- the generated 'mocked'.
data Boxed e = MkBoxed {boxImpl :: forall a. Typeable a => a -> Eff e [a]}

makeMockableCapability [t|Boxed|]

polyResultSpec :: SpecWith ()
polyResultSpec = describe "Bluefin (polymorphic results)" $ do
  it "mocks an operation with a Typeable result" $ do
    x <- runEff $ \io -> runMock io $ \source -> do
      p <- newMock source
      expect p $ PolyResult_ anything |=> \(PolyResult a) -> return (a :: Int)
      polyResultImpl (mocked (mapHandle p)) (3 :: Int)
    x `shouldBe` 3

  it "leaves an operation without a Typeable result to the instance" $ do
    s <- runEff $ \io -> runMock io $ \source -> do
      p <- newMock source
      nonTypeableImpl (mocked (mapHandle p)) "passed through"
    s `shouldBe` "passed through"

  it "derives the instance for an operation with a Typeable result" $ do
    xs <- runEff $ \io -> runMock io $ \source -> do
      b <- newMock source
      -- The result type is a variable, so the expectation has to say which
      -- instance of the operation it means.
      expect b $
        (Box_ anything :: Matcher Boxed "box" String)
          |=> \(Box a) -> return [a, a]
      boxImpl (mocked (mapHandle b)) 'x'
    xs `shouldBe` "xx"

-- All specs. ------------------------------------------------------------------

-- | Every spec in this module.
bluefinTHSpec :: SpecWith ()
bluefinTHSpec = do
  mixedSpec
  namingSpec
  gadtSpec
  callbackSpec
  defaultlessSpec
  polyResultSpec
