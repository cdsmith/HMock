{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Control.Exception (SomeException, evaluate)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Default (Default (def))
import Data.Dynamic (Typeable)
import Data.Kind (Type)
import Data.List (isInfixOf)
import Language.Haskell.TH.Syntax hiding (Type)
import QuasiMock
import Test.HMock
import Test.Hspec
import Test.Predicates (anything, eq, hasSubstr, qMatch, with)
import Util.DeriveRecursive (deriveRecursive)

#if MIN_VERSION_template_haskell(2, 16, 0)
-- Pre-define low-level instance to prevent deriveRecursive from trying.
instance NFData Bytes where rnf = undefined
#endif

deriveRecursive (Just AnyclassStrategy) ''NFData ''Dec

anyQMonadFailure :: SomeException -> Bool
anyQMonadFailure e = "(Q monad failure)" `isInfixOf` show e

class MonadSimple m where
  simple :: String -> m ()

makeMockable [t|MonadSimple|]

simpleTests :: SpecWith ()
simpleTests = describe "MonadSimple" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadSimple)
        runQ (makeMockable [t|MonadSimple|])
      evaluate (rnf decs)

  it "doesn't require unnecessary extensions for simple cases" $
    example . runMockT $ do
      allowUnexpected $ QReifyInstances_ anything anything |-> []
      $(onReify [|expectAny|] ''MonadSimple)
      expectAny $ QIsExtEnabled RankNTypes |-> False

      _ <- runQ (makeMockable [t|MonadSimple|])
      return ()

  it "fails when GADTs is disabled" $
    example $ do
      let missingGADTs = runMockT $ do
            expectAny $ QIsExtEnabled GADTs |-> False
            expect $ QReport_ anything (hasSubstr "Please enable GADTs")

            _ <- runQ (makeMockable [t|MonadSimple|])
            return ()

      missingGADTs `shouldThrow` anyQMonadFailure

  it "fails when TypeFamilies is disabled" $
    example $ do
      let missingTypeFamilies = runMockT $ do
            expectAny $ QIsExtEnabled TypeFamilies |-> False
            expect $ QReport_ anything (hasSubstr "Please enable TypeFamilies")

            _ <- runQ (makeMockable [t|MonadSimple|])
            return ()

      missingTypeFamilies `shouldThrow` anyQMonadFailure

  it "fails when DataKinds is disabled" $
    example $ do
      let missingDataKinds = runMockT $ do
            expectAny $ QIsExtEnabled DataKinds |-> False
            expect $ QReport_ anything (hasSubstr "Please enable DataKinds")

            _ <- runQ (makeMockable [t|MonadSimple|])
            return ()

      missingDataKinds `shouldThrow` anyQMonadFailure

  it "fails when FlexibleInstances is disabled" $
    example $ do
      let missingDataKinds = runMockT $ do
            expectAny $ QIsExtEnabled FlexibleInstances |-> False
            expect $
              QReport_ anything (hasSubstr "Please enable FlexibleInstances")

            _ <- runQ (makeMockable [t|MonadSimple|])
            return ()

      missingDataKinds `shouldThrow` anyQMonadFailure

  it "fails when MultiParamTypeClasses is disabled" $
    example $ do
      let missingDataKinds = runMockT $ do
            expectAny $ QIsExtEnabled MultiParamTypeClasses |-> False
            expect $
              QReport_
                anything
                (hasSubstr "Please enable MultiParamTypeClasses")

            _ <- runQ (makeMockable [t|MonadSimple|])
            return ()

      missingDataKinds `shouldThrow` anyQMonadFailure

  it "fails when ScopedTypeVariables is disabled" $
    example $ do
      let missingDataKinds = runMockT $ do
            expectAny $ QIsExtEnabled ScopedTypeVariables |-> False
            expect $
              QReport_
                anything
                (hasSubstr "Please enable ScopedTypeVariables")

            _ <- runQ (makeMockable [t|MonadSimple|])
            return ()

      missingDataKinds `shouldThrow` anyQMonadFailure

  it "fails when too many params are given" $
    example $ do
      let tooManyParams = runMockT $ do
            $(onReify [|expectAny|] ''MonadSimple)
            $(onReifyInstances [|expectAny|] ''MockableBase [[t|MonadSimple|]])
            $(onReifyInstances [|expectAny|] ''Mockable [[t|MonadSimple|]])
            expect $
              QReport_ anything (hasSubstr "is applied to too many arguments")

            _ <- runQ (makeMockable [t|MonadSimple IO|])
            return ()

      tooManyParams `shouldThrow` anyQMonadFailure

  it "is mockable" $
    example $ do
      let success = runMockT $ do
            expect $ Simple "foo"
            simple "foo"

          failure = runMockT $ do
            expect $ Simple "foo"
            simple "bar"

      success
      failure `shouldThrow` anyException

class MonadSuffix m where
  suffix :: String -> m ()

makeMockableWithOptions [t|MonadSuffix|] def {mockSuffix = "Blah"}

suffixTests :: SpecWith ()
suffixTests = describe "MonadSuffix" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadSuffix)
        runQ $
          makeMockableWithOptions [t|MonadSuffix|] def {mockSuffix = "Blah"}
      evaluate (rnf decs)

  it "is mockable" $
    example $ do
      let success = runMockT $ do
            expect $ SuffixBlah "foo"
            suffix "foo"

          failure = runMockT $ do
            expect $ SuffixBlah "foo"
            suffix "bar"

      success
      failure `shouldThrow` anyException

class MonadWithSetup m where
  withSetup :: m String

makeMockableWithOptions [t|MonadWithSetup|] def {mockEmptySetup = False}

instance Mockable MonadWithSetup where
  setupMockable _ = do
    allowUnexpected $ WithSetup |-> "custom default"

setupTests :: SpecWith ()
setupTests = describe "MonadWithSetup" $ do
  it "generates mock impl" $ do
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadWithSetup)
        runQ $
          makeMockableWithOptions
            [t|MonadWithSetup|]
            def {mockEmptySetup = False}
      evaluate (rnf decs)

  it "returns the customized default value" $ do
    example $
      runMockT $ do
        result <- withSetup
        liftIO (result `shouldBe` "custom default")

  it "uses default in a nested context" $ do
    example $
      runMockT $ do
        nestMockT $ do
          result <- withSetup
          liftIO (result `shouldBe` "custom default")

  it "retains default when initialized in nested context" $ do
    example $
      runMockT $ do
        nestMockT $ do
          _ <- withSetup
          return ()
        result <- withSetup

        liftIO (result `shouldBe` "custom default")

class SuperClass (m :: Type -> Type)

instance SuperClass m

class (SuperClass m, Monad m, Typeable m) => MonadSuper m where
  withSuper :: m ()

makeMockable [t|MonadSuper|]

superTests :: SpecWith ()
superTests = describe "MonadSuper" $ do
  it "generated mock impl" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadSuper)
        expectAny $
          QReifyInstances_
            (eq ''SuperClass)
            $(qMatch [p|[AppT (ConT (Name (OccName "MockT") _)) (VarT _)]|])
            |-> $( reifyInstancesStatic
                     ''SuperClass
                     [AppT (ConT ''MockT) (VarT (mkName "v"))]
                 )

        runQ (makeMockable [t|MonadSuper|])
      evaluate (rnf decs)

  it "is mockable" $
    example $ do
      let success = runMockT $ do
            expect WithSuper
            withSuper

          failure = runMockT withSuper

      success
      failure `shouldThrow` anyException

class MonadMPTC a m where
  mptc :: a -> m ()
  mptcList :: [a] -> m ()

makeMockable [t|MonadMPTC|]

mptcTests :: SpecWith ()
mptcTests = describe "MonadMPTC" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadMPTC)
        runQ (makeMockable [t|MonadMPTC|])
      evaluate (rnf decs)

  it "is mockable" $
    example $ do
      let success = runMockT $ do
            expect $ Mptc "foo"
            mptc "foo"

          failure = runMockT $ do
            expect $ Mptc "foo"
            mptc "bar"

      success
      failure `shouldThrow` anyException

class MonadFDSpecialized a m | m -> a where
  fdSpecialized :: a -> m a

makeMockable [t|MonadFDSpecialized String|]

fdSpecializedTests :: SpecWith ()
fdSpecializedTests = describe "MonadFDSpecialized" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadFDSpecialized)
        runQ (makeMockable [t|MonadFDSpecialized String|])
      evaluate (rnf decs)

  it "is mockable" $
    example $ do
      let success = runMockT $ do
            expect $ FdSpecialized "foo" |-> "bar"
            r <- fdSpecialized "foo"
            liftIO $ r `shouldBe` "bar"

          failure = runMockT $ do
            expect $ FdSpecialized "foo" |-> "bar"
            fdSpecialized "bar"

      success
      failure `shouldThrow` anyException

class MonadFDGeneral a m | m -> a where
  fdGeneral :: a -> m a

makeMockableWithOptions [t|MonadFDGeneral|] def {mockDeriveForMockT = False}

newtype MyBase m a = MyBase {runMyBase :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  (MonadIO m, Typeable m) =>
  MonadFDGeneral String (MockT (MyBase m))
  where
  fdGeneral x = mockMethod (FdGeneral x)

fdGeneralTests :: SpecWith ()
fdGeneralTests = describe "MonadFDGeneral" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadFDGeneral)
        runQ $
          makeMockableWithOptions
            [t|MonadFDGeneral|]
            def {mockDeriveForMockT = False}
      evaluate (rnf decs)

  it "is mockable" $
    example $ do
      let success = runMyBase . runMockT $ do
            expect $ FdGeneral "foo" |-> "bar"
            r <- fdGeneral "foo"
            liftIO $ r `shouldBe` "bar"

          failure = runMyBase . runMockT $ do
            expect $ FdGeneral "foo" |-> "bar"
            fdGeneral "bar"

      success
      failure `shouldThrow` anyException

class MonadFDMixed a b c d m | m -> a b c d where
  fdMixed :: a -> b -> c -> m d

makeMockableWithOptions
  [t|MonadFDMixed String Int|]
  def {mockDeriveForMockT = False}

makeMockable [t|MonadFDMixed String Int String String|]

fdMixedTests :: SpecWith ()
fdMixedTests = describe "MonadFDMixed" $ do
  it "generates mock impl" $
    example . runMockT $ do
      allowUnexpected $ QReifyInstances_ anything anything |-> []
      $(onReify [|expectAny|] ''MonadFDMixed)

      decs1 <-
        runQ $
          makeMockableWithOptions
            [t|MonadFDMixed String Int|]
            def {mockDeriveForMockT = False}
      decs2 <-
        runQ (makeMockable [t|MonadFDMixed String Int String Int|])
      _ <- liftIO $ evaluate (rnf (decs1 ++ decs2))
      return ()

  it "is mockable" $
    example $ do
      let success = runMockT $ do
            expect $ FdMixed "foo" (1 :: Int) "bar" |-> "qux"
            r <- fdMixed "foo" 1 "bar"
            liftIO $ r `shouldBe` "qux"

          failure = runMockT $ do
            expect $ FdMixed "foo" (1 :: Int) "bar" |-> "qux"
            _ <- fdMixed "bar" 1 "foo"
            return ()

      success
      failure `shouldThrow` anyException

class MonadPolyArg m where
  polyArg :: Enum a => String -> a -> b -> m ()

makeMockable [t|MonadPolyArg|]

polyArgTests :: SpecWith ()
polyArgTests = describe "MonadPolyArg" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadPolyArg)
        runQ (makeMockable [t|MonadPolyArg|])
      evaluate (rnf decs)

  it "fails without RankNTypes" $
    example $ do
      let missingRankNTypes = runMockT $ do
            $(onReify [|expectAny|] ''MonadPolyArg)
            $(onReifyInstances [|expectAny|] ''MockableBase [[t|MonadPolyArg|]])
            expectAny $ QIsExtEnabled RankNTypes |-> False
            expect $
              QReport_ anything (hasSubstr "Please enable RankNTypes")

            _ <- runQ (makeMockable [t|MonadPolyArg|])
            return ()

      missingRankNTypes `shouldThrow` anyQMonadFailure

  it "is mockable" $
    example $ do
      let success = runMyBase . runMockT $ do
            expect $
              PolyArg_ (eq "foo") (with fromEnum (eq 1)) anything
            polyArg "foo" (toEnum 1 :: Bool) "hello"

          failure = runMyBase . runMockT $ do
            expect $
              PolyArg_ (eq "foo") (with fromEnum (eq 1)) anything
            polyArg "foo" (toEnum 2 :: Bool) "hello"

      success
      failure `shouldThrow` anyException

class MonadUnshowableArg m where
  unshowableArg :: (Int -> Int) -> m ()

makeMockable [t|MonadUnshowableArg|]

unshowableArgTests :: SpecWith ()
unshowableArgTests = describe "MonadUnshowableArg" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadUnshowableArg)
        runQ (makeMockable [t|MonadUnshowableArg|])
      evaluate (rnf decs)

  it "is mockable" $
    example $ do
      let success = runMyBase . runMockT $ do
            expect $ UnshowableArg_ anything
            unshowableArg (+ 1)

          failure = runMyBase . runMockT $ do
            expect $ UnshowableArg_ anything

            unshowableArg (+ 1)
            unshowableArg (+ 1)

      success
      failure `shouldThrow` anyException

class MonadInArg m where
  monadInArg :: (Int -> m ()) -> m ()

makeMockable [t|MonadInArg|]

monadInArgTests :: SpecWith ()
monadInArgTests = describe "MonadInArg" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadInArg)
        runQ (makeMockable [t|MonadInArg|])
      evaluate (rnf decs)

  it "is mockable" $
    example $ do
      let success = runMyBase . runMockT $ do
            expect $ MonadInArg_ anything
            monadInArg (const (return ()))

          failure = runMyBase . runMockT $ do
            expect $ UnshowableArg_ anything

            monadInArg (const (return ()))
            monadInArg (const (return ()))

      success
      failure `shouldThrow` anyException

class MonadPolyResult m where
  polyResult :: Typeable a => a -> m a
  nonTypeablePolyResult :: a -> m a

makeMockableWithOptions [t|MonadPolyResult|] def {mockDeriveForMockT = False}

instance (MonadIO m, Typeable m) => MonadPolyResult (MockT m) where
  polyResult x = mockDefaultlessMethod (PolyResult x)
  nonTypeablePolyResult x = return x

polyResultTests :: SpecWith ()
polyResultTests = describe "MonadPolyResult" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadPolyResult)
        runQ $
          makeMockableWithOptions
            [t|MonadPolyResult|]
            def {mockDeriveForMockT = False}
      evaluate (rnf decs)

  it "is mockable" $
    example . runMockT $ do
      expect $ PolyResult_ anything |=> \(PolyResult a) -> return (a :: Int)
      x <- polyResult (3 :: Int)
      liftIO $ x `shouldBe` 3

class MonadDefaultSignatures m where
  methodWithDefault :: Int -> m Int

  default methodWithDefault :: MonadIO m => Int -> m Int
  methodWithDefault = liftIO . methodWithDefault

instance MonadDefaultSignatures IO where
  methodWithDefault = pure

makeMockable [t|MonadDefaultSignatures|]

defaultSignaturesTests :: SpecWith ()
defaultSignaturesTests = describe "MonadDefaultSignatures" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadDefaultSignatures)
        runQ (makeMockable [t|MonadDefaultSignatures|])
      evaluate (rnf decs)
  it "is mockable" $
    example . runMockT $ do
      expect $ MethodWithDefault_ anything |=> \MethodWithDefault{} -> return 42
      x <- methodWithDefault 0
      liftIO $ x `shouldBe` 42

class MonadExtraneousMembers m where
  data SomeDataType m
  favoriteNumber :: SomeDataType m -> Int
  wrongMonad :: Monad n => m Int -> n Int
  nestedRankN :: ((forall a. a -> Bool) -> Bool) -> m ()

  mockableMethod :: Int -> m ()

makeMockableWithOptions
  [t|MonadExtraneousMembers|]
  def {mockDeriveForMockT = False}

instance (Typeable m, MonadIO m) => MonadExtraneousMembers (MockT m) where
  data SomeDataType (MockT m) = SomeCon
  favoriteNumber SomeCon = 42
  wrongMonad _ = return 42
  nestedRankN _ = return ()

  mockableMethod a = mockMethod (MockableMethod a)

extraneousMembersTests :: SpecWith ()
extraneousMembersTests = describe "MonadExtraneousMembers" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadExtraneousMembers)
        runQ $
          makeMockableWithOptions
            [t|MonadExtraneousMembers|]
            def {mockDeriveForMockT = False}
      evaluate (rnf decs)

  it "warns about non-methods" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadExtraneousMembers)
        expect $
          QReport
            False
            "SomeDataType must be defined manually in MockT instance."
        expect $
          QReport
            False
            ( "favoriteNumber can't be mocked: "
                ++ "return value not in the expected monad."
            )
        expect $
          QReport
            False
            ( "wrongMonad can't be mocked: "
                ++ "return value not in the expected monad."
            )
        expect $
          QReport
            False
            "nestedRankN can't be mocked: rank-n types nested in arguments."

        runQ
          ( makeMockableWithOptions
              [t|MonadExtraneousMembers|]
              def {mockDeriveForMockT = False, mockVerbose = True}
          )
      evaluate (rnf decs)

  it "fails to derive MockT when class has extra methods" $
    example $ do
      let unmockableMethods = runMockT $ do
            $(onReify [|expectAny|] ''MonadExtraneousMembers)
            $( onReifyInstances
                 [|expectAny|]
                 ''MockableBase
                 [[t|MonadExtraneousMembers|]]
             )
            $( onReifyInstances
                 [|expectAny|]
                 ''Mockable
                 [[t|MonadExtraneousMembers|]]
             )
            expect $
              QReport_ anything (hasSubstr "has unmockable methods")

            _ <- runQ (makeMockable [t|MonadExtraneousMembers|])
            return ()

      unmockableMethods `shouldThrow` anyQMonadFailure

  it "is mockable" $
    example $ do
      let success = runMyBase . runMockT $ do
            expect $ MockableMethod 42
            mockableMethod (favoriteNumber (SomeCon @(MockT IO)))

          failure = runMyBase . runMockT $ do
            expect $ MockableMethod 42
            mockableMethod 12

      success
      failure `shouldThrow` anyException

class MonadRankN m where
  rankN :: (forall a. a -> Bool) -> Bool -> m ()

makeMockable [t|MonadRankN|]

rankNTests :: SpecWith ()
rankNTests = describe "MonadRankN" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadRankN)
        runQ (makeMockable [t|MonadRankN|])
      evaluate (rnf decs)

  it "is mockable" $
    example $ do
      let success = runMockT $ do
            expect $ RankN_ anything (eq True)
            rankN (const True) True

          failure = runMockT $ do
            expect $ RankN_ anything (eq True)
            rankN (const True) False

      success
      failure `shouldThrow` anyException

-- | Type with no Default instance.
data NoDefault = NoDefault

class MonadManyReturns m where
  returnsUnit :: m ()
  returnsInt :: m Int
  returnsString :: m String
  returnsMaybe :: m (Maybe Bool)
  returnsNoDefault :: m NoDefault

makeMockable [t|MonadManyReturns|]

defaultReturnTests :: SpecWith ()
defaultReturnTests = do
  describe "MonadManyReturns" $ do
    it "generates mock impl" $
      example $ do
        decs <- runMockT $ do
          allowUnexpected $ QReifyInstances_ anything anything |-> []
          $(onReify [|expectAny|] ''MonadManyReturns)
          $(onReifyInstances [|expectAny|] ''Default [[t|NoDefault|]])

          runQ (makeMockable [t|MonadManyReturns|])
        evaluate (rnf decs)

    it "fails when there's an unexpected method" $
      example $ runMockT returnsUnit `shouldThrow` anyException

    it "succeeds when there's an expected method with default response" $
      example $ do
        result <- runMockT $ do
          expect ReturnsUnit
          expect ReturnsInt
          expect ReturnsString
          expect ReturnsMaybe

          (,,,)
            <$> returnsUnit
            <*> returnsInt
            <*> returnsString
            <*> returnsMaybe

        result `shouldBe` ((), 0, "", Nothing)

    it "overrides default when response is specified" $
      example $ do
        result <- runMockT $ do
          expect ReturnsInt
          expect $ ReturnsString |-> "non-default"

          (,)
            <$> returnsInt
            <*> returnsString

        result `shouldBe` (0, "non-default")

    it "returns undefined when response isn't given for defaultless method" $
      example $ do
        let test = runMockT $ do
              expect ReturnsNoDefault
              returnsNoDefault
        _ <- test
        (test >>= evaluate) `shouldThrow` anyException

class MonadNestedNoDef m where
  nestedNoDef :: m (NoDefault, String)

makeMockable [t|MonadNestedNoDef|]

nestedNoDefTests :: SpecWith ()
nestedNoDefTests = describe "MonadNestedNoDef" $ do
  it "generates mock impl" $
    example $ do
      decs <- runMockT $ do
        allowUnexpected $ QReifyInstances_ anything anything |-> []
        $(onReify [|expectAny|] ''MonadNestedNoDef)
        $(onReifyInstances [|expectAny|] ''Default [[t|(NoDefault, String)|]])
        $(onReifyInstances [|expectAny|] ''Default [[t|NoDefault|]])

        runQ (makeMockable [t|MonadNestedNoDef|])
      evaluate (rnf decs)

class ClassWithNoParams

$(return []) -- Hack to get types into the TH environment.

errorTests :: SpecWith ()
errorTests = describe "errors" $ do
  it "fails when given a type instead of a class" $
    example $ do
      let wrongKind = runMockT $ do
            expect $
              QReport_
                anything
                (hasSubstr "Expected GHC.Types.Int to be a class")

            _ <- runQ (makeMockable [t|Int|])
            return ()

      wrongKind `shouldThrow` anyQMonadFailure

  it "fails when given an unexpected type constructor" $
    example $ do
      let notClass = runMockT $ do
            expect $ QReport_ anything (hasSubstr "Expected a class")

            _ <- runQ (makeMockable [t|(Int, String)|])
            return ()

      notClass `shouldThrow` anyQMonadFailure

  it "fails when class has no params" $
    example $ do
      let tooManyParams = runMockT $ do
            $(onReify [|expectAny|] ''ClassWithNoParams)
            expect $
              QReport_
                anything
                (hasSubstr "ClassWithNoParams has no type parameters")

            _ <- runQ (makeMockable [t|ClassWithNoParams|])
            return ()

      tooManyParams `shouldThrow` anyQMonadFailure

  it "fails when class has no mockable methods" $
    example $ do
      let noMockableMethods = runMockT $ do
            $(onReify [|expectAny|] ''Show)
            expect $ QReport_ anything (hasSubstr "has no mockable methods")
            _ <- runQ (makeMockable [t|Show|])
            return ()

      noMockableMethods `shouldThrow` anyQMonadFailure

classTests :: SpecWith ()
classTests = describe "makeMockable" $ do
  simpleTests
  suffixTests
  setupTests
  superTests
  mptcTests
  fdSpecializedTests
  fdGeneralTests
  fdMixedTests
  polyArgTests
  unshowableArgTests
  monadInArgTests
  defaultSignaturesTests
  extraneousMembersTests
  rankNTests
  defaultReturnTests
  nestedNoDefTests
  errorTests
