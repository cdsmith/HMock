{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Monad.Trans (liftIO)
import Language.Haskell.TH
import QuasiMock
import Test.HMock
import Test.HMock.Internal.TH (resolveInstance)
import Test.Hspec

data NotShowable

$(pure [])

thUtilSpec :: SpecWith ()
thUtilSpec = do
  describe "resolveInstance" $ do
    it "finds unrestricted instances" $
      example $
        runMockT $ do
          result <- runQ $ resolveInstance ''Show [ConT ''String]
          liftIO $ result `shouldBe` Just []

    it "finds context on nested vars" $
      example $
        runMockT $ do
          v <- runQ $ newName "a"
          result <- runQ $ resolveInstance ''Show [AppT ListT (VarT v)]
          liftIO $ result `shouldBe` Just [AppT (ConT ''Show) (VarT v)]

    it "recognizes when a nested type lacks instance" $
      example $
        runMockT $ do
          $(onReify [|expectAny|] ''NotShowable)
          $(onReifyInstances [|expectAny|] ''Show [ConT ''NotShowable])
          $( [t|(Int, NotShowable)|]
               >>= onReifyInstances [|expectAny|] ''Show . (: [])
           )

          t <- runQ [t|(Int, NotShowable)|]
          result <- runQ $ resolveInstance ''Show [t]
          liftIO $ result `shouldBe` Nothing
