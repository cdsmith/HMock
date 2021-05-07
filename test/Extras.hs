{-# LANGUAGE TypeApplications #-}

module Extras where

import Data.List (isPrefixOf)
import HMock
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

        accept (suchThat ((> 5) . length)) "lengthy" `shouldBe` True
        accept (suchThat ((> 5) . length)) "short" `shouldBe` False

        accept (typed @String anything) () `shouldBe` False
        accept (typed @String (eq "foo")) "bar" `shouldBe` False
        accept (typed @String (eq "foo")) "foo" `shouldBe` True

    it "describes itself" $
      example $ do
        showPredicate anything `shouldBe` "anything"
        showPredicate (eq "foo") `shouldBe` "\"foo\""
        showPredicate (neq "foo") `shouldBe` "≠ \"foo\""
        showPredicate (lt "foo") `shouldBe` "< \"foo\""
        showPredicate (gt "foo") `shouldBe` "> \"foo\""
        showPredicate (leq "foo") `shouldBe` "≤ \"foo\""
        showPredicate (geq "foo") `shouldBe` "≥ \"foo\""
        showPredicate (lt "foo" `andP` gt "bar")
          `shouldBe` "< \"foo\" and > \"bar\""
        showPredicate (lt "bar" `orP` gt "foo")
          `shouldBe` "< \"bar\" or > \"foo\""
        showPredicate (notP (gt "foo")) `shouldBe` "not > \"foo\""
        showPredicate (hasSubstr "i") `shouldBe` "has substring \"i\""
        showPredicate (startsWith "fun") `shouldBe` "starts with \"fun\""
        showPredicate (endsWith "ing") `shouldBe` "ends with \"ing\""
        showPredicate (suchThat ((> 5) . length) :: Predicate String)
          `shouldSatisfy` ("custom predicate at " `isPrefixOf`)
        showPredicate (typed @String anything :: Predicate Int)
          `shouldBe` "anything :: [Char]"
        showPredicate (typed @String (eq "foo") :: Predicate Int)
          `shouldBe` "\"foo\" :: [Char]"
