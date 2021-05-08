{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Classes where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import Data.Default (Default (def))
import Data.Dynamic (Typeable)
import Language.Haskell.TH (Extension (..), runQ)
import QuasiMock
import THUtil (reifyStatic)
import Test.HMock
import Test.HMock.Mockable (mockMethod)
import Test.HMock.TH
import Test.Hspec

class MonadSimple m where
  simple :: String -> m ()

makeMockable ''MonadSimple

simpleTests :: SpecWith ()
simpleTests = describe "MonadSimple" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        setupQuasi
        whenever $ qReify_ ''MonadSimple |-> $(reifyStatic ''MonadSimple)

        runQ (makeMockable ''MonadSimple)
      evaluate (rnf decs)

  it "doesn't require unnecessary extensions for simple cases" $
    example . runMockT $ do
      setupQuasi
      whenever $ qReify_ ''MonadSimple |-> $(reifyStatic ''MonadSimple)
      expectAny $ qIsExtEnabled_ FlexibleInstances |-> False
      expectAny $ qIsExtEnabled_ ScopedTypeVariables |-> False
      expectAny $ qIsExtEnabled_ RankNTypes |-> False

      _ <- runQ (makeMockable ''MonadSimple)
      return ()

  it "fails when GADTs is disabled" $
    example $ do
      let missingGADTs = runMockT $ do
            setupQuasi
            expectAny $ qIsExtEnabled_ GADTs |-> False
            expect $ QReport_ anything (hasSubstr "Please enable GADTs") |-> ()

            _ <- runQ (makeMockable ''MonadSimple)
            return ()

      missingGADTs `shouldThrow` anyIOException

  it "fails when TypeFamilies is disabled" $
    example $ do
      let missingTypeFamilies = runMockT $ do
            setupQuasi
            expectAny $ qIsExtEnabled_ TypeFamilies |-> False
            expect $
              QReport_ anything (hasSubstr "Please enable TypeFamilies") |-> ()

            _ <- runQ (makeMockable ''MonadSimple)
            return ()

      missingTypeFamilies `shouldThrow` anyIOException

  it "fails when DataKinds is disabled" $
    example $ do
      let missingDataKinds = runMockT $ do
            setupQuasi
            expectAny $ qIsExtEnabled_ DataKinds |-> False
            expect $
              QReport_ anything (hasSubstr "Please enable DataKinds") |-> ()

            _ <- runQ (makeMockable ''MonadSimple)
            return ()

      missingDataKinds `shouldThrow` anyIOException

  it "fails when too many params are given" $
    example $ do
      let tooManyParams = runMockT $ do
            setupQuasi
            whenever $ qReify_ ''MonadSimple |-> $(reifyStatic ''MonadSimple)
            expect $
              QReport_ anything (hasSubstr "is applied to too many arguments")
                |-> ()

            _ <- runQ (makeMockableType [t|MonadSimple IO|])
            return ()

      tooManyParams `shouldThrow` anyIOException

  it "generates mock impl with a suffix" $
    example $ do
      decs <- runMockT $ do
        setupQuasi
        whenever $ qReify_ ''MonadSimple |-> $(reifyStatic ''MonadSimple)

        runQ (makeMockableWithOptions def {mockSuffix = "Blah"} ''MonadSimple)
      evaluate (rnf decs)

  it "is mockable" $
    example $ do
      let success = runMockT $ do
            expect $ simple_ "foo" |-> ()
            simple "foo"

          failure = runMockT $ do
            expect $ simple_ "foo" |-> ()
            simple "bar"

      success
      failure `shouldThrow` anyException

class MonadMPTC a m where
  mptc :: a -> m ()

makeMockable ''MonadMPTC

mptcTests :: SpecWith ()
mptcTests = describe "MonadMPTC" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        setupQuasi
        whenever $ qReify_ ''MonadMPTC |-> $(reifyStatic ''MonadMPTC)

        runQ (makeMockable ''MonadMPTC)
      evaluate (rnf decs)

  it "is mockable" $
    example $ do
      let success = runMockT $ do
            expect $ mptc_ "foo" |-> ()
            mptc "foo"

          failure = runMockT $ do
            expect $ mptc_ "foo" |-> ()
            mptc "bar"

      success
      failure `shouldThrow` anyException

class MonadFDSpecialized a m | m -> a where
  fdSpecialized :: a -> m ()

makeMockableType [t|MonadFDSpecialized String|]

fdSpecializedTests :: SpecWith ()
fdSpecializedTests = describe "MonadFDSpecialized" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        setupQuasi
        whenever $
          qReify_ ''MonadFDSpecialized
            |-> $(reifyStatic ''MonadFDSpecialized)

        runQ (makeMockableType [t|MonadFDSpecialized String|])
      evaluate (rnf decs)

  it "fails without FlexibleInstances" $
    example $ do
      let missingFlexibleInstances = runMockT $ do
            setupQuasi
            whenever $
              qReify_ ''MonadFDSpecialized
                |-> $(reifyStatic ''MonadFDSpecialized)
            expectAny $ qIsExtEnabled_ FlexibleInstances |-> False
            expect $
              QReport_ anything (hasSubstr "Please enable FlexibleInstances")
                |-> ()

            _ <- runQ (makeMockableType [t|MonadFDSpecialized String|])
            return ()

      missingFlexibleInstances `shouldThrow` anyIOException

  it "is mockable" $
    example $ do
      let success = runMockT $ do
            expect $ fdSpecialized_ "foo" |-> ()
            fdSpecialized "foo"

          failure = runMockT $ do
            expect $ fdSpecialized_ "foo" |-> ()
            fdSpecialized "bar"

      success
      failure `shouldThrow` anyException

class MonadFDGeneral a m | m -> a where
  fdGeneral :: a -> m ()

deriveMockable ''MonadFDGeneral

newtype MyBase m a = MyBase {runMyBase :: m a}
  deriving (Functor, Applicative, Monad)

instance
  (Monad m, Typeable m) =>
  MonadFDGeneral String (MockT (MyBase m))
  where
  fdGeneral x = mockMethod (FdGeneral x)

fdGeneralTests :: SpecWith ()
fdGeneralTests = describe "MonadFDGeneral" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        setupQuasi
        whenever $ qReify_ ''MonadFDGeneral |-> $(reifyStatic ''MonadFDGeneral)

        runQ (deriveMockable ''MonadFDGeneral)
      evaluate (rnf decs)

  it "is mockable" $
    example $ do
      let success = runMyBase . runMockT $ do
            expect $ fdGeneral_ "foo" |-> ()
            fdGeneral "foo"

          failure = runMyBase . runMockT $ do
            expect $ fdGeneral_ "foo" |-> ()
            fdGeneral "bar"

      success
      failure `shouldThrow` anyException

class MonadFDMixed a b c d m | m -> a b c d where
  fdMixed :: a -> b -> c -> m d

deriveMockableType [t|MonadFDMixed String Int|]
deriveTypeForMockT [t|MonadFDMixed String Int String ()|]

fdMixedTests :: SpecWith ()
fdMixedTests = describe "MonadFDMixed" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        setupQuasi
        whenever $ qReify_ ''MonadFDMixed |-> $(reifyStatic ''MonadFDMixed)

        decs1 <- runQ (deriveMockableType [t|MonadFDMixed String Int|])
        decs2 <-
          runQ (deriveTypeForMockT [t|MonadFDMixed String Int String ()|])
        return (decs1 ++ decs2)
      evaluate (rnf decs)

  it "is mockable" $
    example $ do
      let success = runMockT $ do
            expect $ fdMixed_ "foo" 1 "bar" |-> ()
            fdMixed "foo" 1 "bar"

          failure = runMockT $ do
            expect $ fdMixed_ "foo" 1 "bar" |-> ()
            fdMixed "bar" 1 "foo"

      success
      failure `shouldThrow` anyException

class MonadPolyArg m where
  polyArg :: Enum a => String -> a -> b -> m ()

makeMockable ''MonadPolyArg

polyArgTests :: SpecWith ()
polyArgTests = describe "MonadPolyArg" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        setupQuasi
        whenever $ qReify_ ''MonadPolyArg |-> $(reifyStatic ''MonadPolyArg)

        runQ (makeMockable ''MonadPolyArg)
      evaluate (rnf decs)

  it "fails without ScopedTypeVariables" $
    example $ do
      let missingScopedTypeVariables = runMockT $ do
            setupQuasi
            whenever $
              qReify_ ''MonadPolyArg |-> $(reifyStatic ''MonadPolyArg)
            expectAny $ qIsExtEnabled_ ScopedTypeVariables |-> False
            expect $
              QReport_ anything (hasSubstr "Please enable ScopedTypeVariables")
                |-> ()

            _ <- runQ (makeMockable ''MonadPolyArg)
            return ()

      missingScopedTypeVariables `shouldThrow` anyException

  it "fails without RankNTypes" $
    example $ do
      let missingRankNTypes = runMockT $ do
            setupQuasi
            whenever $ qReify_ ''MonadPolyArg |-> $(reifyStatic ''MonadPolyArg)
            expectAny $ qIsExtEnabled_ RankNTypes |-> False
            expect $
              QReport_ anything (hasSubstr "Please enable RankNTypes") |-> ()

            _ <- runQ (makeMockable ''MonadPolyArg)
            return ()

      missingRankNTypes `shouldThrow` anyException

  it "is mockable" $
    example $ do
      let success = runMyBase . runMockT $ do
            expect $
              PolyArg_ (eq "foo") (suchThat ((== 1) . fromEnum)) anything |-> ()
            polyArg "foo" (toEnum 1 :: Bool) "hello"

          failure = runMyBase . runMockT $ do
            expect $
              PolyArg_ (eq "foo") (suchThat ((== 1) . fromEnum)) anything |-> ()
            polyArg "foo" (toEnum 2 :: Bool) "hello"

      success
      failure `shouldThrow` anyException

class MonadUnshowableArg m where
  unshowableArg :: (Int -> Int) -> m ()

makeMockable ''MonadUnshowableArg

unshowableArgTests :: SpecWith ()
unshowableArgTests = describe "MonadUnshowableArg" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        setupQuasi
        whenever $
          qReify_ ''MonadUnshowableArg |-> $(reifyStatic ''MonadUnshowableArg)

        runQ (makeMockable ''MonadUnshowableArg)
      evaluate (rnf decs)

  it "is mockable" $
    example $ do
      let success = runMyBase . runMockT $ do
            expect $ UnshowableArg_ anything |-> ()
            unshowableArg (+ 1)

          failure = runMyBase . runMockT $ do
            expect $ UnshowableArg_ anything |-> ()

            unshowableArg (+ 1)
            unshowableArg (+ 1)

      success
      failure `shouldThrow` anyException

class MonadInArg m where
  monadInArg :: (Int -> m ()) -> m ()

makeMockable ''MonadInArg

monadInArgTests :: SpecWith ()
monadInArgTests = describe "MonadInArg" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        setupQuasi
        whenever $ qReify_ ''MonadInArg |-> $(reifyStatic ''MonadInArg)

        runQ (makeMockable ''MonadInArg)
      evaluate (rnf decs)

  it "is mockable" $
    example $ do
      let success = runMyBase . runMockT $ do
            expect $ MonadInArg_ anything |-> ()
            monadInArg (const (return ()))

          failure = runMyBase . runMockT $ do
            expect $ UnshowableArg_ anything |-> ()

            monadInArg (const (return ()))
            monadInArg (const (return ()))

      success
      failure `shouldThrow` anyException

class MonadExtraneousMembers m where
  data SomeDataType m
  favoriteNumber :: SomeDataType m -> Int

  mockableMethod :: Int -> m ()

deriveMockable ''MonadExtraneousMembers

instance (Typeable m, Monad m) => MonadExtraneousMembers (MockT m) where
  data SomeDataType (MockT m) = SomeCon
  favoriteNumber SomeCon = 42
  mockableMethod a = mockMethod (MockableMethod a)

extraneousMembersTests :: SpecWith ()
extraneousMembersTests = describe "MonadExtraneousMembers" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        setupQuasi
        whenever $
          qReify_ ''MonadExtraneousMembers
            |-> $(reifyStatic ''MonadExtraneousMembers)

        runQ (deriveMockable ''MonadExtraneousMembers)
      evaluate (rnf decs)

  it "fails to derive MockT when class has extra methods" $
    example $ do
      let unmockableMethods = runMockT $ do
            setupQuasi
            whenever $
              qReify_ ''MonadExtraneousMembers
                |-> $(reifyStatic ''MonadExtraneousMembers)
            expect $
              QReport_ anything (hasSubstr "has unmockable methods") |-> ()

            _ <- runQ (makeMockable ''MonadExtraneousMembers)
            return ()

      unmockableMethods `shouldThrow` anyIOException

  it "is mockable" $
    example $ do
      let success = runMyBase . runMockT $ do
            expect $ mockableMethod_ 42 |-> ()
            mockableMethod (favoriteNumber (SomeCon @(MockT IO)))

          failure = runMyBase . runMockT $ do
            expect $ mockableMethod_ 42 |-> ()
            mockableMethod 12

      success
      failure `shouldThrow` anyException

class ClassWithNoParams

$(return []) -- Hack to get the error classes into the TH environment.

errorTests :: SpecWith ()
errorTests = describe "errors" $ do
  it "fails when given a type instead of a class" $
    example $ do
      let wrongKind = runMockT $ do
            setupQuasi
            whenever $ qReify_ ''Int |-> $(reifyStatic ''Int)
            expect $
              QReport_
                anything
                (hasSubstr "Expected GHC.Types.Int to be a class")
                |-> ()

            _ <- runQ (makeMockable ''Int)
            return ()

      wrongKind `shouldThrow` anyIOException

  it "fails when given an unexpected type construct" $
    example $ do
      let notClass = runMockT $ do
            setupQuasi
            expect $ QReport_ anything (hasSubstr "Expected a class") |-> ()

            _ <- runQ (makeMockableType [t|(Int, String)|])
            return ()

      notClass `shouldThrow` anyIOException

  it "fails when class has no params" $
    example $ do
      let tooManyParams = runMockT $ do
            setupQuasi
            whenever $
              qReify_ ''ClassWithNoParams |-> $(reifyStatic ''ClassWithNoParams)
            expect $
              QReport_
                anything
                (hasSubstr "ClassWithNoParams has no type parameters")
                |-> ()

            _ <- runQ (makeMockable ''ClassWithNoParams)
            return ()

      tooManyParams `shouldThrow` anyIOException

  it "fails when class has no mockable methods" $
    example $ do
      let noMockableMethods = runMockT $ do
            setupQuasi
            whenever $ qReify_ ''Show |-> $(reifyStatic ''Show)
            expect $
              QReport_ anything (hasSubstr "has no mockable methods") |-> ()

            _ <- runQ (deriveMockable ''Show)
            return ()

      noMockableMethods `shouldThrow` anyIOException

classTests :: SpecWith ()
classTests = describe "makeMockable" $ do
  simpleTests
  mptcTests
  fdSpecializedTests
  fdGeneralTests
  fdMixedTests
  polyArgTests
  unshowableArgTests
  monadInArgTests
  extraneousMembersTests
  errorTests
