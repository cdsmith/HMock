{-# LANGUAGE TypeApplications #-}

module Extras where

import Data.List (isPrefixOf)
import Data.Typeable (Typeable)
import Test.HMock
import Test.Hspec

cardinalityTests :: SpecWith ()
cardinalityTests = do
  describe "Cardinality" $
    it "describes itself" $
      example $ do
        show once `shouldBe` "once"
        show anyCardinality `shouldBe` "any number of times"
        show (exactly 2) `shouldBe` "twice"
        show (exactly 3) `shouldBe` "3 times"
        show (atLeast 1) `shouldBe` "at least once"
        show (atLeast 2) `shouldBe` "at least twice"
        show (atLeast 3) `shouldBe` "at least 3 times"
        show (atMost 1) `shouldBe` "at most once"
        show (atMost 2) `shouldBe` "at most twice"
        show (atMost 3) `shouldBe` "at most 3 times"
        show (interval 2 3) `shouldBe` "2 or 3 times"
        show (interval 2 5) `shouldBe` "2 to 5 times"

predicateTests :: SpecWith ()
predicateTests = do
  describe "Predicate" $ do
    it "accepts the right values" $
      example $ do
        accept anything "foo" `shouldBe` True

        accept (eq "foo") "foo" `shouldBe` True
        accept (eq "foo") "bar" `shouldBe` False

        accept (neq "foo") "foo" `shouldBe` False
        accept (neq "foo") "bar" `shouldBe` True

        accept (lt "foo") "bar" `shouldBe` True
        accept (lt "foo") "foo" `shouldBe` False
        accept (lt "foo") "quz" `shouldBe` False

        accept (gt "foo") "bar" `shouldBe` False
        accept (gt "foo") "foo" `shouldBe` False
        accept (gt "foo") "quz" `shouldBe` True

        accept (leq "foo") "bar" `shouldBe` True
        accept (leq "foo") "foo" `shouldBe` True
        accept (leq "foo") "quz" `shouldBe` False

        accept (geq "foo") "bar" `shouldBe` False
        accept (geq "foo") "foo" `shouldBe` True
        accept (geq "foo") "quz" `shouldBe` True

        accept (lt "foo" `andP` gt "bar") "eta" `shouldBe` True
        accept (lt "foo" `andP` gt "bar") "quz" `shouldBe` False
        accept (lt "foo" `andP` gt "bar") "alpha" `shouldBe` False

        accept (lt "bar" `orP` gt "foo") "eta" `shouldBe` False
        accept (lt "bar" `orP` gt "foo") "quz" `shouldBe` True
        accept (lt "bar" `orP` gt "foo") "alpha" `shouldBe` True

        accept (notP (gt "foo")) "bar" `shouldBe` True
        accept (notP (gt "foo")) "quz" `shouldBe` False

        accept (hasSubstr "i") "team" `shouldBe` False
        accept (hasSubstr "ea") "team" `shouldBe` True

        accept (startsWith "fun") "fungible" `shouldBe` True
        accept (startsWith "gib") "fungible" `shouldBe` False

        accept (endsWith "ing") "yearning" `shouldBe` True
        accept (endsWith "ed") "burnt" `shouldBe` False

        accept (size (gt 2)) ["a", "b", "c"] `shouldBe` True
        accept (size (gt 2)) ["a", "b"] `shouldBe` False

        accept (elems [gt "a", lt "b"]) ["c", "a"] `shouldBe` True
        accept (elems [gt "a", lt "b"]) ["c", "c"] `shouldBe` False

        accept (allElems (gt "a")) ["c", "b"] `shouldBe` True
        accept (allElems (gt "a")) ["a", "b"] `shouldBe` False

        accept (anyElem (gt "a")) ["a", "b"] `shouldBe` True
        accept (anyElem (gt "a")) ["a", "a"] `shouldBe` False

        accept (suchThat ((> 5) . length)) "lengthy" `shouldBe` True
        accept (suchThat ((> 5) . length)) "short" `shouldBe` False

    it "describes itself" $
      example $ do
        show anything `shouldBe` "anything"
        show (eq "foo") `shouldBe` "\"foo\""
        show (neq "foo") `shouldBe` "≠ \"foo\""
        show (lt "foo") `shouldBe` "< \"foo\""
        show (gt "foo") `shouldBe` "> \"foo\""
        show (leq "foo") `shouldBe` "≤ \"foo\""
        show (geq "foo") `shouldBe` "≥ \"foo\""
        show (lt "foo" `andP` gt "bar")
          `shouldBe` "< \"foo\" and > \"bar\""
        show (lt "bar" `orP` gt "foo")
          `shouldBe` "< \"bar\" or > \"foo\""
        show (notP (gt "foo")) `shouldBe` "not > \"foo\""
        show (hasSubstr "i") `shouldBe` "has substring \"i\""
        show (startsWith "fun") `shouldBe` "starts with \"fun\""
        show (endsWith "ing") `shouldBe` "ends with \"ing\""
        show (size (gt 5) :: Predicate [()]) `shouldBe` "size > 5"
        show (elems [gt 5, eq 5] :: Predicate [Int]) `shouldBe` "[> 5, 5]"
        show (allElems (gt 5) :: Predicate [Int]) `shouldBe` "all > 5"
        show (anyElem (gt 5) :: Predicate [Int]) `shouldBe` "any > 5"
        show (suchThat ((> 5) . length) :: Predicate String)
          `shouldSatisfy` ("custom predicate at " `isPrefixOf`)

    it "checks types" $
      example $ do
        let p1 :: Typeable a => Predicate a
            p1 = typed @String anything

            p2 :: Typeable a => Predicate a
            p2 = typed @String (eq "foo")

        show (p1 :: Predicate String)
          `shouldBe` "anything :: [Char]"
        show (p1 :: Predicate Int)
          `shouldBe` "anything :: [Char]"

        accept p1 "foo" `shouldBe` True
        accept p1 "bar" `shouldBe` True
        accept p1 () `shouldBe` False
        accept p1 (5 :: Int) `shouldBe` False

        show (p2 :: Predicate String)
          `shouldBe` "\"foo\" :: [Char]"
        show (p2 :: Predicate Int)
          `shouldBe` "\"foo\" :: [Char]"

        accept p2 "foo" `shouldBe` True
        accept p2 "bar" `shouldBe` False
        accept p2 (5 :: Int) `shouldBe` False
