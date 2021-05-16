{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Extras where

import Data.List (isPrefixOf)
import Data.Typeable (Typeable)
import Test.HMock
import Test.Hspec

multiplicityTests :: SpecWith ()
multiplicityTests = do
  describe "Multiplicity" $
    it "describes itself" $
      example $ do
        show once `shouldBe` "once"
        show anyMultiplicity `shouldBe` "any number of times"
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

        accept (gt "foo") "bar" `shouldBe` False
        accept (gt "foo") "foo" `shouldBe` False
        accept (gt "foo") "quz" `shouldBe` True

        accept (geq "foo") "bar" `shouldBe` False
        accept (geq "foo") "foo" `shouldBe` True
        accept (geq "foo") "quz" `shouldBe` True

        accept (lt "foo") "bar" `shouldBe` True
        accept (lt "foo") "foo" `shouldBe` False
        accept (lt "foo") "quz" `shouldBe` False

        accept (leq "foo") "bar" `shouldBe` True
        accept (leq "foo") "foo" `shouldBe` True
        accept (leq "foo") "quz" `shouldBe` False

        accept (just (gt "foo")) Nothing `shouldBe` False
        accept (just (gt "foo")) (Just "bar") `shouldBe` False
        accept (just (gt "foo")) (Just "qux") `shouldBe` True

        accept (left (gt "foo")) (Left "qux") `shouldBe` True
        accept (left (gt "foo")) (Left "bar") `shouldBe` False
        accept (left (gt "foo")) (Right "qux") `shouldBe` False

        accept (right (gt "foo")) (Left "qux") `shouldBe` False
        accept (right (gt "foo")) (Right "bar") `shouldBe` False
        accept (right (gt "foo")) (Right "qux") `shouldBe` True

        accept (lt "foo" `andP` gt "bar") "eta" `shouldBe` True
        accept (lt "foo" `andP` gt "bar") "quz" `shouldBe` False
        accept (lt "foo" `andP` gt "bar") "alpha" `shouldBe` False

        accept (lt "bar" `orP` gt "foo") "eta" `shouldBe` False
        accept (lt "bar" `orP` gt "foo") "quz" `shouldBe` True
        accept (lt "bar" `orP` gt "foo") "alpha" `shouldBe` True

        accept (notP (gt "foo")) "bar" `shouldBe` True
        accept (notP (gt "foo")) "quz" `shouldBe` False

        accept (startsWith "fun") "fungible" `shouldBe` True
        accept (startsWith "gib") "fungible" `shouldBe` False

        accept (endsWith "ing") "yearning" `shouldBe` True
        accept (endsWith "ed") "burnt" `shouldBe` False

        accept (hasSubstr "i") "team" `shouldBe` False
        accept (hasSubstr "ea") "team" `shouldBe` True

        accept (hasSubsequence "abc") "abc" `shouldBe` True
        accept (hasSubsequence "abc") "zazbzcz" `shouldBe` True
        accept (hasSubsequence "abc") "cba" `shouldBe` False

        accept (caseInsensitive eq "foo") "foo" `shouldBe` True
        accept (caseInsensitive eq "foo") "FOO" `shouldBe` True
        accept (caseInsensitive eq "foo") "bar" `shouldBe` False
        accept (caseInsensitive startsWith "foo") "FOOTBALL" `shouldBe` True
        accept (caseInsensitive startsWith "foo") "SOCCER" `shouldBe` False

        accept isEmpty "" `shouldBe` True
        accept isEmpty "foo" `shouldBe` False

        accept nonEmpty "" `shouldBe` False
        accept nonEmpty "foo" `shouldBe` True

        accept (sizeIs (gt 2)) ["a", "b", "c"] `shouldBe` True
        accept (sizeIs (gt 2)) ["a", "b"] `shouldBe` False

        accept (elemsAre [gt "a", lt "b"]) ["c", "a"] `shouldBe` True
        accept (elemsAre [gt "a", lt "b"]) ["c", "c"] `shouldBe` False
        accept (elemsAre [gt "a", lt "b"]) ["c"] `shouldBe` False

        accept (unorderedElemsAre [gt "n", lt "d"]) ["a", "z"] `shouldBe` True
        accept (unorderedElemsAre [gt "n", lt "d"]) ["z", "a"] `shouldBe` True
        accept (unorderedElemsAre [gt "n", lt "d"]) ["a", "a"] `shouldBe` False
        accept (unorderedElemsAre [lt "d"]) ["a", "a"] `shouldBe` False

        accept (each (gt "a")) ["c", "b"] `shouldBe` True
        accept (each (gt "a")) ["a", "b"] `shouldBe` False

        accept (contains (gt "a")) ["a", "b"] `shouldBe` True
        accept (contains (gt "a")) ["a", "a"] `shouldBe` False

        accept
          (containsAll [eq "foo", caseInsensitive eq "bar"])
          ["qux", "BAR", "foo"]
          `shouldBe` True
        accept
          (containsAll [eq "foo", caseInsensitive eq "bar"])
          ["qux", "bar", "FOO"]
          `shouldBe` False

        accept
          (containsOnly [eq "foo", caseInsensitive eq "bar"])
          ["foo", "bar", "BAR"]
          `shouldBe` True
        accept
          (containsOnly [eq "foo", caseInsensitive eq "bar"])
          ["foo", "bar", "FOO"]
          `shouldBe` False

        -- Example of floating point rounding error:
        pi * 11 / 11 `shouldNotBe` (pi :: Double)

        accept (approxEq pi) (pi * 11 / 11 :: Double) `shouldBe` True
        accept (approxEq pi) (3 :: Double) `shouldBe` False

        accept finite (1.0 :: Double) `shouldBe` True
        accept finite (1.0 / 0.0 :: Double) `shouldBe` False
        accept finite (0.0 / 0.0 :: Double) `shouldBe` False

        accept infinite (1.0 :: Double) `shouldBe` False
        accept infinite (1.0 / 0.0 :: Double) `shouldBe` True
        accept infinite (0.0 / 0.0 :: Double) `shouldBe` False

        accept nAn (1.0 :: Double) `shouldBe` False
        accept nAn (1.0 / 0.0 :: Double) `shouldBe` False
        accept nAn (0.0 / 0.0 :: Double) `shouldBe` True

        accept (suchThat ((> 5) . length)) "lengthy" `shouldBe` True
        accept (suchThat ((> 5) . length)) "short" `shouldBe` False

    it "describes itself" $
      example $ do
        show anything `shouldBe` "anything"
        show (eq "foo") `shouldBe` "\"foo\""
        show (neq "foo") `shouldBe` "≠ \"foo\""
        show (gt "foo") `shouldBe` "> \"foo\""
        show (geq "foo") `shouldBe` "≥ \"foo\""
        show (lt "foo") `shouldBe` "< \"foo\""
        show (leq "foo") `shouldBe` "≤ \"foo\""
        show (just (gt "foo")) `shouldBe` "Just (> \"foo\")"
        show (left (gt "foo")) `shouldBe` "Left (> \"foo\")"
        show (right (gt "foo")) `shouldBe` "Right (> \"foo\")"
        show (lt "foo" `andP` gt "bar")
          `shouldBe` "< \"foo\" and > \"bar\""
        show (lt "bar" `orP` gt "foo")
          `shouldBe` "< \"bar\" or > \"foo\""
        show (notP (gt "foo")) `shouldBe` "not > \"foo\""
        show (startsWith "fun") `shouldBe` "starts with \"fun\""
        show (endsWith "ing") `shouldBe` "ends with \"ing\""
        show (hasSubstr "i") `shouldBe` "has substring \"i\""
        show (hasSubsequence "abc") `shouldBe` "has subsequence \"abc\""
        show (caseInsensitive eq "foo") `shouldBe` "(case insensitive) \"foo\""
        show (caseInsensitive startsWith "foo")
          `shouldBe` "(case insensitive) starts with \"foo\""
        show (caseInsensitive endsWith "foo")
          `shouldBe` "(case insensitive) ends with \"foo\""
        show (isEmpty :: Predicate [()]) `shouldBe` "empty"
        show (nonEmpty :: Predicate [()]) `shouldBe` "nonempty"
        show (sizeIs (gt 5) :: Predicate [()]) `shouldBe` "size > 5"
        show (elemsAre [gt 5, eq 5] :: Predicate [Int]) `shouldBe` "[> 5,5]"
        show (unorderedElemsAre [gt 5, eq 5] :: Predicate [Int])
          `shouldBe` "(any order) [> 5,5]"
        show (each (gt 5) :: Predicate [Int]) `shouldBe` "each (> 5)"
        show (contains (gt 5) :: Predicate [Int]) `shouldBe` "contains (> 5)"
        show (containsAll [gt 5] :: Predicate [Int])
          `shouldBe` "contains all of [> 5]"
        show (containsOnly [gt 5] :: Predicate [Int])
          `shouldBe` "contains only [> 5]"
        show (approxEq 1.0 :: Predicate Double) `shouldBe` "≈ 1.0"
        show (finite :: Predicate Double) `shouldBe` "finite"
        show (infinite :: Predicate Double) `shouldBe` "infinite"
        show (nAn :: Predicate Double) `shouldBe` "NaN"
        show (suchThat ((> 5) . length) :: Predicate String)
          `shouldSatisfy` ("custom predicate at " `isPrefixOf`)

    it "matches patterns" $
      example $ do
        let p = $(match [p|Just (Left _)|])
        show p `shouldBe` "Just (Left _)"

        accept p (Just (Left "foo")) `shouldBe` True
        accept p (Just (Right "foo")) `shouldBe` False
        accept p Nothing `shouldBe` False

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
