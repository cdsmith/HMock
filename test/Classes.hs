{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Classes where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Default (Default (def))
import Data.Dynamic (Typeable)
import Language.Haskell.TH.Syntax
import QuasiMock
import THUtil (deriveRecursive)
import Test.HMock
import Test.HMock.TH
import Test.Hspec

#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Fail (MonadFail)
#endif

#if MIN_VERSION_template_haskell(2, 16, 0)

-- Pre-define low-level instance to prevent deriveRecursive from trying.
instance NFData Bytes where rnf = undefined

#endif

deriveRecursive (Just AnyclassStrategy) ''NFData ''Dec

-- | Sets up some common default behaviors for the Quasi type class.
setupQuasi :: (Typeable m, MonadIO m, MonadFail m) => MockT m ()
setupQuasi = do
  whenever $ QIsExtEnabled_ anything |-> True

  whenever $ qReify_ ''String |-> $(reifyStatic ''String)
  whenever $ qReify_ ''Int |-> $(reifyStatic ''Int)
  whenever $ qReify_ ''Bool |-> $(reifyStatic ''Bool)
  whenever $ qReify_ ''Enum |-> $(reifyStatic ''Enum)
  whenever $ qReify_ ''Monad |-> $(reifyStatic ''Monad)
  whenever $
    qReifyInstances_ ''Show [ConT ''String]
      |-> $(reifyInstancesStatic ''Show [ConT ''String])
  whenever $
    qReifyInstances_ ''Eq [ConT ''String]
      |-> $(reifyInstancesStatic ''Eq [ConT ''String])
  whenever $
    qReifyInstances_ ''Show [ConT ''Int]
      |-> $(reifyInstancesStatic ''Show [ConT ''Int])
  whenever $
    qReifyInstances_ ''Eq [ConT ''Int]
      |-> $(reifyInstancesStatic ''Eq [ConT ''Int])
  whenever $
    qReifyInstances_ ''Show [ConT ''Bool]
      |-> $(reifyInstancesStatic ''Show [ConT ''Bool])
  whenever $
    qReifyInstances_ ''Eq [ConT ''Bool]
      |-> $(reifyInstancesStatic ''Eq [ConT ''Bool])
  whenever $
    QReifyInstances_ (eq ''Show) (is functionType) |-> []
  whenever $
    QReifyInstances_ (eq ''Eq) (is functionType) |-> []

  whenever $
    QReifyInstances_ (eq ''Show) (elemsAre [$(qMatch [p|AppT ListT (VarT _)|])])
      |-> $(reifyInstancesStatic ''Show [AppT ListT (VarT (mkName "a_0"))])
  whenever $
    QReifyInstances_ (eq ''Eq) (elemsAre [$(qMatch [p|AppT ListT (VarT _)|])])
      |-> $(reifyInstancesStatic ''Eq [AppT ListT (VarT (mkName "a_0"))])

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
      expectAny $ qIsExtEnabled_ FlexibleContexts |-> False

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

class MonadSuffix m where
  suffix :: String -> m ()

makeMockableWithOptions def {mockSuffix = "Blah"} ''MonadSuffix

suffixTests :: SpecWith ()
suffixTests = describe "MonadSuffix" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        setupQuasi
        whenever $ qReify_ ''MonadSuffix |-> $(reifyStatic ''MonadSuffix)

        runQ (makeMockableWithOptions def {mockSuffix = "Blah"} ''MonadSuffix)
      evaluate (rnf decs)

  it "is mockable" $
    example $ do
      let success = runMockT $ do
            expect $ suffixBlah_ "foo" |-> ()
            suffix "foo"

          failure = runMockT $ do
            expect $ suffixBlah_ "foo" |-> ()
            suffix "bar"

      success
      failure `shouldThrow` anyException

class (MonadIO m, Monad m, Typeable m) => MonadSuper m where
  withSuper :: m ()

makeMockable ''MonadSuper

superTests :: SpecWith ()
superTests = describe "MonadSuper" $ do
  it "generated mock impl" $
    example $ do
      decs <- runMockT $ do
        setupQuasi
        whenever $ qReify_ ''MonadSuper |-> $(reifyStatic ''MonadSuper)

        runQ (makeMockable ''MonadSuper)
      evaluate (rnf decs)

  it "fails when FlexibleContexts is disabled" $ do
    let missingFlexibleContexts = runMockT $ do
          setupQuasi
          expectAny $ qIsExtEnabled_ FlexibleContexts |-> False
          whenever $ qReify_ ''MonadSuper |-> $(reifyStatic ''MonadSuper)
          expect $
            QReport_ anything (hasSubstr "Please enable FlexibleContexts")
              |-> ()

          _ <- runQ (makeMockable ''MonadSuper)
          return ()

    missingFlexibleContexts `shouldThrow` anyIOException

  it "is mockable" $
    example $ do
      let success = runMockT $ do
            expect $ withSuper_ |-> ()
            withSuper

          failure = runMockT withSuper

      success
      failure `shouldThrow` anyException

class MonadMPTC a m where
  mptc :: a -> m ()
  mptcList :: [a] -> m ()

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
  fdSpecialized :: a -> m a

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
            expect $ fdSpecialized_ "foo" |-> "bar"
            r <- fdSpecialized "foo"
            liftIO $ r `shouldBe` "bar"

          failure = runMockT $ do
            expect $ fdSpecialized_ "foo" |-> "bar"
            fdSpecialized "bar"

      success
      failure `shouldThrow` anyException

class MonadFDGeneral a m | m -> a where
  fdGeneral :: a -> m a

deriveMockable ''MonadFDGeneral

newtype MyBase m a = MyBase {runMyBase :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

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
            expect $ fdGeneral_ "foo" |-> "bar"
            r <- fdGeneral "foo"
            liftIO $ r `shouldBe` "bar"

          failure = runMyBase . runMockT $ do
            expect $ fdGeneral_ "foo" |-> "bar"
            fdGeneral "bar"

      success
      failure `shouldThrow` anyException

class MonadFDMixed a b c d m | m -> a b c d where
  fdMixed :: a -> b -> c -> m d

deriveMockableType [t|MonadFDMixed String Int|]
deriveTypeForMockT [t|MonadFDMixed String Int String String|]

fdMixedTests :: SpecWith ()
fdMixedTests = describe "MonadFDMixed" $ do
  it "generates mock impl" $
    example . runMockT $ do
        setupQuasi
        whenever $ qReify_ ''MonadFDMixed |-> $(reifyStatic ''MonadFDMixed)

        decs1 <- runQ (deriveMockableType [t|MonadFDMixed String Int|])
        decs2 <-
          runQ (deriveTypeForMockT [t|MonadFDMixed String Int String Int|])
        _ <- liftIO $ evaluate (rnf (decs1 ++ decs2))
        return ()

  it "is mockable" $
    example $ do
      let success = runMockT $ do
            expect $ fdMixed_ "foo" 1 "bar" |-> "qux"
            r <- fdMixed "foo" 1 "bar"
            liftIO $ r `shouldBe` "qux"

          failure = runMockT $ do
            expect $ fdMixed_ "foo" 1 "bar" |-> "qux"
            _ <- fdMixed "bar" 1 "foo"
            return ()

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
              PolyArg_ (eq "foo") (with fromEnum (eq 1)) anything |-> ()
            polyArg "foo" (toEnum 1 :: Bool) "hello"

          failure = runMyBase . runMockT $ do
            expect $
              PolyArg_ (eq "foo") (with fromEnum (eq 1)) anything |-> ()
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
  wrongMonad :: Monad n => m Int -> n Int
  polyResult :: a -> m a
  nestedRankN :: ((forall a. a -> Bool) -> Bool) -> m ()

  mockableMethod :: Int -> m ()

deriveMockable ''MonadExtraneousMembers

instance (Typeable m, Monad m) => MonadExtraneousMembers (MockT m) where
  data SomeDataType (MockT m) = SomeCon
  favoriteNumber SomeCon = 42
  wrongMonad _ = return 42
  polyResult = return
  nestedRankN _ = return ()

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

  it "warns about non-methods" $
    example $ do
      decs <- runMockT $ do
        setupQuasi
        whenever $
          qReify_ ''MonadExtraneousMembers
            |-> $(reifyStatic ''MonadExtraneousMembers)
        expect $ qReport_ False "A non-value member cannot be mocked." |-> ()
        expect $
          qReport_ False "favoriteNumber can't be mocked: non-monadic result."
            |-> ()
        expect $
          qReport_
            False
            "wrongMonad can't be mocked: return value in wrong monad."
            |-> ()
        expect $
          qReport_ False "polyResult can't be mocked: polymorphic return value."
            |-> ()
        expect $
          qReport_
            False
            "nestedRankN can't be mocked: rank-n types nested in arguments."
            |-> ()

        runQ
          ( deriveMockableWithOptions
              def {mockVerbose = True}
              ''MonadExtraneousMembers
          )
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

class MonadRankN m where
  rankN :: (forall a. a -> Bool) -> Bool -> m ()

makeMockable ''MonadRankN

rankNTests :: SpecWith ()
rankNTests = describe "MonadRankN" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        setupQuasi
        whenever $
          qReify_ ''MonadRankN
            |-> $(reifyStatic ''MonadRankN)

        runQ (deriveMockable ''MonadRankN)
      evaluate (rnf decs)

  it "is mockable" $ do
    example $ do
      let success = runMockT $ do
            expect $ RankN_ anything (eq True) |-> ()
            rankN (const True) True

          failure = runMockT $ do
            expect $ RankN_ anything (eq True) |-> ()
            rankN (const True) False

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
  suffixTests
  superTests
  mptcTests
  fdSpecializedTests
  fdGeneralTests
  fdMixedTests
  polyArgTests
  unshowableArgTests
  monadInArgTests
  extraneousMembersTests
  rankNTests
  errorTests
