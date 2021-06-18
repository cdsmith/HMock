{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Typeable (Typeable)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import QuasiMock
import Test.HMock
import Test.HMock.Internal.TH.Util
import Test.Hspec

data NotShowable

$(pure [])

setup :: (Typeable m, MonadIO m) => MockT m ()
setup = do
  expectAny $ QReify ''Bool |-> $(reifyStatic ''Bool)
  expectAny $ QReify ''Int |-> $(reifyStatic ''Int)
  expectAny $ QReify ''String |-> $(reifyStatic ''String)
  expectAny $ QReify ''NotShowable |-> $(reifyStatic ''NotShowable)

  expectAny $
    QReifyInstances ''Show [ConT ''String]
      |-> $(reifyInstancesStatic ''Show [ConT ''String])
  expectAny $
    QReifyInstances ''Show [ConT ''Char]
      |-> $(reifyInstancesStatic ''Show [ConT ''Char])
  expectAny $
    QReifyInstances ''Show [ConT ''Int]
      |-> $(reifyInstancesStatic ''Show [ConT ''Int])
  expectAny $
    QReifyInstances ''Show [ConT ''NotShowable]
      |-> $(reifyInstancesStatic ''Show [ConT ''NotShowable])
  expectAny $
    QReifyInstances_
      (eq ''Show)
      (elemsAre [$(qMatch [p|AppT ListT (VarT _)|])])
      |-> $( reifyInstancesStatic
               ''Show
               [AppT ListT (VarT (Name (OccName "v") NameS))]
           )
  expectAny $
    QReifyInstances
      ''Show
      [AppT (AppT (TupleT 2) (ConT ''Int)) (ConT ''NotShowable)]
      |-> $( reifyInstancesStatic
               ''Show
               [AppT (AppT (TupleT 2) (ConT ''Int)) (ConT ''NotShowable)]
           )

thUtilSpec :: SpecWith ()
thUtilSpec = do
  describe "unifyTypes" $ do
    it "accepts identical types" $
      example $
        runMockT $ do
          setup

          result <- runQ $ unifyTypes (ConT ''Bool) (ConT ''Bool)
          liftIO $ result `shouldBe` Just []

    it "rejects different types" $
      example $
        runMockT $ do
          setup

          result <- runQ $ unifyTypes (ConT ''Bool) (ConT ''Int)
          liftIO $ result `shouldBe` Nothing

    it "unifies patterns with variables" $
      example $
        runMockT $ do
          setup

          v <- runQ $ newName "a"
          t1 <- runQ [t|Maybe ($(varT v), Int)|]
          t2 <- runQ [t|Maybe (Bool, Int)|]
          result <- runQ $ unifyTypes t1 t2
          liftIO $ result `shouldBe` Just [(v, ConT ''Bool)]

    it "substitutes synonyms" $
      example $
        runMockT $ do
          setup

          v <- runQ $ newName "a"
          t1 <- runQ [t|[$(varT v)]|]
          t2 <- runQ [t|String|]
          result <- runQ $ unifyTypes t1 t2
          liftIO $ result `shouldBe` Just [(v, ConT ''Char)]

  describe "resolveInstance" $ do
    it "finds unrestricted instances" $
      example $
        runMockT $ do
          setup

          result <- runQ $ resolveInstance ''Show (ConT ''String)
          liftIO $ result `shouldBe` Just []

    it "finds context on nested vars" $
      example $
        runMockT $ do
          setup

          v <- runQ $ newName "a"
          result <- runQ $ resolveInstance ''Show (AppT ListT (VarT v))
          liftIO $ result `shouldBe` Just [AppT (ConT ''Show) (VarT v)]

    it "recognizes when a nested type lacks instance" $
      example $
        runMockT $ do
          setup

          t <- runQ [t|(Int, NotShowable)|]
          result <- runQ $ resolveInstance ''Show t
          liftIO $ result `shouldBe` Nothing
