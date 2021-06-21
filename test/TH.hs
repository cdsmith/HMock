{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Monad.Trans (liftIO)
import Language.Haskell.TH
import QuasiMock
import Test.HMock
import Test.HMock.Internal.TH.Util (resolveInstance, unifyTypes)
import Test.Hspec

data NotShowable

$(pure [])

thUtilSpec :: SpecWith ()
thUtilSpec = do
  describe "unifyTypes" $ do
    it "accepts identical types" $
      example $
        runMockT $ do
          result <- runQ $ unifyTypes (ConT ''Bool) (ConT ''Bool)
          liftIO $ result `shouldBe` Just []

    it "rejects different types" $
      example $
        runMockT $ do
          result <- runQ $ unifyTypes (ConT ''Bool) (ConT ''Int)
          liftIO $ result `shouldBe` Nothing

    it "unifies patterns with variables" $
      example $
        runMockT $ do
          v <- runQ $ newName "a"
          t1 <- runQ [t|Maybe ($(varT v), Int)|]
          t2 <- runQ [t|Maybe (Bool, Int)|]
          result <- runQ $ unifyTypes t1 t2
          liftIO $ result `shouldBe` Just [(v, ConT ''Bool)]

    it "substitutes synonyms" $
      example $
        runMockT $ do
          v <- runQ $ newName "a"
          t1 <- runQ [t|[$(varT v)]|]
          t2 <- runQ [t|String|]
          result <- runQ $ unifyTypes t1 t2
          liftIO $ result `shouldBe` Just [(v, ConT ''Char)]

  describe "resolveInstance" $ do
    it "finds unrestricted instances" $
      example $
        runMockT $ do
          result <- runQ $ resolveInstance ''Show (ConT ''String)
          liftIO $ result `shouldBe` Just []

    it "finds context on nested vars" $
      example $
        runMockT $ do
          v <- runQ $ newName "a"
          result <- runQ $ resolveInstance ''Show (AppT ListT (VarT v))
          liftIO $ result `shouldBe` Just [AppT (ConT ''Show) (VarT v)]

    it "recognizes when a nested type lacks instance" $
      example $
        runMockT $ do
          $(expectReify ''NotShowable)
          $(expectReifyInstances ''Show [ConT ''NotShowable])
          $( [t|(Int, NotShowable)|] >>= expectReifyInstances ''Show . (:[]))

          t <- runQ [t|(Int, NotShowable)|]
          result <- runQ $ resolveInstance ''Show t
          liftIO $ result `shouldBe` Nothing
