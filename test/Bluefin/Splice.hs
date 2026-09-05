{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Runs 'makeMockableCapability' through the mocked 'Quasi' monad, as
-- "Classic.Classes" does for 'makeMockable', to check the shapes of
-- capability declaration it accepts and the messages it gives for the ones
-- it rejects.
module Bluefin.Splice
  ( bluefinSpliceSpec,
    Counter (..),
    Store (..),
    Backwards (..),
    Nameless (..),
    Twofold (..),
    Constrained (..),
    Untypeable (..),
    ScopedArg (..),
    Mixed (..),
    Clashing (..),
  )
where

import Bluefin.Eff (Eff, Effects, type (:&))
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Data.Default (def)
import Language.Haskell.TH (Type (AppT, ConT))
import Language.Haskell.TH.Syntax (runQ)
import Support.QuasiMock
import Test.HMock
import Test.HMock.Bluefin.TH
  ( MakeMockableCapabilityOptions (..),
    makeMockableCapability,
    makeMockableCapabilityWithOptions,
  )
import Test.Hspec
import Test.Predicates (anything, eq, hasSubstr)

-- | A capability declared as a newtype in GADT record syntax.
newtype Counter (e :: Effects) where
  MkCounter :: {tickImpl :: Eff e Int} -> Counter e

-- | The same, as a data type with several fields.
data Store (e :: Effects) where
  MkStore ::
    { getImpl :: String -> Eff e (Maybe Int),
      putImpl :: String -> Int -> Eff e ()
    } ->
    Store e

-- | A record whose effect scope isn't its last type parameter, so none of its
-- fields can be mocked.
data Backwards (e :: Effects) k = MkBackwards {lookupImpl :: k -> Eff e Int}

-- | Not a record, so its operation has no name.
data Nameless (e :: Effects) = MkNameless (Eff e Int)

-- | More than one constructor.
data Twofold (e :: Effects)
  = MkOne {oneImpl :: Eff e Int}
  | MkTwo {twoImpl :: Eff e Int}

-- | A constructor with a context.
data Constrained (e :: Effects)
  = forall a. Show a => MkConstrained {showImpl :: a -> Eff e String}

-- | A result whose type variable has no 'Typeable' constraint.
data Untypeable (e :: Effects) = MkUntypeable
  {passImpl :: forall a. a -> Eff e a}

-- | An argument that mentions the scope, but isn't a callback.
data ScopedArg (e :: Effects) = MkScopedArg
  {wrapImpl :: (Eff e Int -> Eff e Int) -> Eff e Int}

-- | One mockable operation, and one whose result isn't in the scope.
data Mixed (e :: Effects) = MkMixed
  { plainImpl :: Int -> Eff e String,
    withScopeImpl :: forall e' a. Eff e' a -> Eff (e' :& e) a
  }

-- | Two fields that are mocked under the same name once the suffix is
-- stripped.
data Clashing (e :: Effects) = MkClashing
  { ping :: Eff e Int,
    pingImpl :: Eff e Int
  }

-- Ends the declaration group, so that the splices below can reify these.
return []

bluefinSpliceSpec :: SpecWith ()
bluefinSpliceSpec = describe "makeMockableCapability" $ do
  it "accepts a newtype declared in GADT record syntax" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''Counter)
        runQ (makeMockableCapability [t|Counter|])
      evaluate (rnf decs)

  it "accepts a data type declared in GADT record syntax" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''Store)
        runQ (makeMockableCapability [t|Store|])
      evaluate (rnf decs)

  it "suggests checking the effect parameter when nothing is mockable" $
    example $ do
      let backwards = runMockT $ do
            $(onReify [|expectAny|] ''Backwards)
            expect $
              QReport_
                anything
                (hasSubstr "Is the effect scope the last parameter?")
            _ <- runQ (makeMockableCapability [t|Backwards|])
            return ()
      backwards `shouldThrow` anyQMonadFailure

  it "rejects a type that isn't a record" $
    example $ do
      let nameless = runMockT $ do
            $(onReify [|expectAny|] ''Nameless)
            expect $ QReport_ anything (hasSubstr "is not a record")
            _ <- runQ (makeMockableCapability [t|Nameless|])
            return ()
      nameless `shouldThrow` anyQMonadFailure

  it "rejects a type with more than one constructor" $
    example $ do
      let twofold = runMockT $ do
            $(onReify [|expectAny|] ''Twofold)
            expect $
              QReport_ anything (hasSubstr "has more than one constructor")
            _ <- runQ (makeMockableCapability [t|Twofold|])
            return ()
      twofold `shouldThrow` anyQMonadFailure

  it "rejects a constructor with a context" $
    example $ do
      let constrained = runMockT $ do
            $(onReify [|expectAny|] ''Constrained)
            expect $ QReport_ anything (hasSubstr "has a context")
            _ <- runQ (makeMockableCapability [t|Constrained|])
            return ()
      constrained `shouldThrow` anyQMonadFailure

  it "rejects a type applied to too many arguments" $
    example $ do
      let overApplied = runMockT $ do
            $(onReify [|expectAny|] ''Counter)
            expect $
              QReport_ anything (hasSubstr "is applied to too many arguments")
            _ <-
              runQ
                ( makeMockableCapability
                    (pure (AppT (ConT ''Counter) (ConT ''Int)))
                )
            return ()
      overApplied `shouldThrow` anyQMonadFailure

  it "explains a result type that isn't Typeable" $
    example $ do
      let untypeable = runMockT $ do
            $(onReify [|expectAny|] ''Untypeable)
            -- The explanation must come before the failure, or the failure
            -- goes unmatched and the test sees an unexpected action instead.
            inSequence
              [ expect $
                  QReport_
                    (eq False)
                    (hasSubstr "result type is not Typeable"),
                expect $ QReport_ (eq True) anything
              ]
            _ <-
              runQ
                ( makeMockableCapabilityWithOptions
                    [t|Untypeable|]
                    def {capVerbose = True}
                )
            return ()
      untypeable `shouldThrow` anyQMonadFailure

  it "explains an argument that mentions the scope without being a callback" $
    example $ do
      let scopedArg = runMockT $ do
            $(onReify [|expectAny|] ''ScopedArg)
            inSequence
              [ expect $
                  QReport_
                    (eq False)
                    (hasSubstr "other than as a callback"),
                expect $ QReport_ (eq True) anything
              ]
            _ <-
              runQ
                ( makeMockableCapabilityWithOptions
                    [t|ScopedArg|]
                    def {capVerbose = True}
                )
            return ()
      scopedArg `shouldThrow` anyQMonadFailure

  it "refuses to derive the instance when an operation can't be mocked" $
    example $ do
      let mixed = runMockT $ do
            allowUnexpected $ QReifyInstances_ anything anything |-> []
            $(onReify [|expectAny|] ''Mixed)
            expect $
              QReport_
                anything
                (hasSubstr "unmockable operations: withScopeImpl")
            _ <- runQ (makeMockableCapability [t|Mixed|])
            return ()
      mixed `shouldThrow` anyQMonadFailure

  it "rejects two operations that would be mocked under one name" $
    example $ do
      let clashing = runMockT $ do
            $(onReify [|expectAny|] ''Clashing)
            expect $
              QReport_
                anything
                (hasSubstr "ping and pingImpl would both be mocked as Ping")
            _ <- runQ (makeMockableCapability [t|Clashing|])
            return ()
      clashing `shouldThrow` anyQMonadFailure

  it "generates the mockable operations when told not to derive the instance" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''Mixed)
        runQ
          ( makeMockableCapabilityWithOptions
              [t|Mixed|]
              def {capDeriveInstance = False}
          )
      evaluate (rnf decs)
