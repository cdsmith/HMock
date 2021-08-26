{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module IfCxt where

import Test.HMock.Internal.IfCxt
import Test.Hspec
import Data.Proxy (Proxy (Proxy))

deriveOptionalShow

ifCxtSpec :: SpecWith ()
ifCxtSpec = describe "ifCxt" $ do
  it "finds simple Show instances" $
      ifCxt (Proxy :: Proxy (Show Int)) "yes" "no" `shouldBe` "yes"
  it "finds complex Show instances" $
      ifCxt (Proxy :: Proxy (Show ((), [Int]))) "yes" "no" `shouldBe` "yes"
  it "rejects simple non-Show types" $
      ifCxt (Proxy :: Proxy (Show (Int -> Int))) "yes" "no" `shouldBe` "no"
  it "rejects complex non-Show types" $
      ifCxt (Proxy :: Proxy (Show [Int -> Int])) "yes" "no" `shouldBe` "no"

optionalShowSpec :: SpecWith ()
optionalShowSpec = describe "optionalShow" $ do
  it "uses Show instance in ifShow" $
    ifShow (12345 :: Int) (take 3) "N/A" `shouldBe` "123"

  it "defaults in ifShow" $
    ifShow (negate :: Int -> Int) (take 3) "N/A" `shouldBe` "N/A"

  it "uses Show instance in showOr" $
    showOr "N/A" (12345 :: Int) `shouldBe` "12345"

  it "defaults in showOr" $
    showOr "N/A" (negate :: Int -> Int) `shouldBe` "N/A"
