{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Extras where

import Data.List (isPrefixOf)
import Data.Typeable (Typeable)
import Test.HMock
import Test.Hspec
import Test.QuickCheck hiding (once)

instance Arbitrary Multiplicity where
  arbitrary =
    between
      <$> (fromInteger <$> arbitrary)
      <*> (fromInteger . getNonNegative <$> arbitrary)

guessMultiplicity :: [Int] -> Multiplicity
guessMultiplicity vals
  | null vals = -1
  | otherwise = between (fromIntegral (minimum vals)) (fromIntegral (maximum vals))

multiplicityTests :: SpecWith ()
multiplicityTests = do
  describe "Multiplicity" $ do
    it "describes itself" $
      example $ do
        show once `shouldBe` "once"
        show anyMultiplicity `shouldBe` "any number of times"
        show (2 :: Multiplicity) `shouldBe` "twice"
        show (3 :: Multiplicity) `shouldBe` "3 times"
        show (atLeast 1) `shouldBe` "at least once"
        show (atLeast 2) `shouldBe` "at least twice"
        show (atLeast 3) `shouldBe` "at least 3 times"
        show (atMost 1) `shouldBe` "at most once"
        show (atMost 2) `shouldBe` "at most twice"
        show (atMost 3) `shouldBe` "at most 3 times"
        show (between 2 3) `shouldBe` "2 or 3 times"
        show (between 2 5) `shouldBe` "2 to 5 times"

    it "correctly implements sum" $
      property $
        \m1 m2 (NonNegative n) ->
          let natSums k = [(i, k - i) | i <- [0 .. k]]
           in meetsMultiplicity (m1 + m2) n
                === any
                  (\(j, k) -> meetsMultiplicity m1 j && meetsMultiplicity m2 k)
                  (natSums n)

    it "correctly implements difference" $
      property $ \m1 m2 ->
        m1 - m2
          === guessMultiplicity
            [ i - j
              | i <- [0 .. 100],
                meetsMultiplicity m1 i,
                j <- [0 .. 100],
                meetsMultiplicity m2 j
            ]

predicateTests :: SpecWith ()
predicateTests = do
  describe "Predicate" $ do
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
        show (zipP (eq 1) (eq 2) :: Predicate (Int, Int)) `shouldBe` "(1,2)"
        show (zip3P (eq 1) (eq 2) (eq 3) :: Predicate (Int, Int, Int))
          `shouldBe` "(1,2,3)"
        show
          ( zip4P (eq 1) (eq 2) (eq 3) (eq 4) ::
              Predicate (Int, Int, Int, Int)
          )
          `shouldBe` "(1,2,3,4)"
        show
          ( zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5) ::
              Predicate (Int, Int, Int, Int, Int)
          )
          `shouldBe` "(1,2,3,4,5)"
        show (lt "foo" `andP` gt "bar")
          `shouldBe` "< \"foo\" and > \"bar\""
        show (lt "bar" `orP` gt "foo")
          `shouldBe` "< \"bar\" or > \"foo\""
        show (notP (gt "foo")) `shouldBe` "≤ \"foo\""
        show (startsWith "fun") `shouldBe` "starts with \"fun\""
        show (endsWith "ing") `shouldBe` "ends with \"ing\""
        show (hasSubstr "i") `shouldBe` "has substring \"i\""
        show (hasSubsequence "abc") `shouldBe` "has subsequence \"abc\""
        show (caseInsensitive eq "foo") `shouldBe` "(case insensitive) \"foo\""
        show (caseInsensitive startsWith "foo")
          `shouldBe` "(case insensitive) starts with \"foo\""
        show (caseInsensitive endsWith "foo")
          `shouldBe` "(case insensitive) ends with \"foo\""
        show (matchesRegex "foo" :: Predicate String)
          `shouldBe` "/foo/"
        show (matchesCaseInsensitiveRegex "foo" :: Predicate String)
          `shouldBe` "/foo/i"
        show (containsRegex "foo" :: Predicate String)
          `shouldBe` "contains /foo/"
        show (containsCaseInsensitiveRegex "foo" :: Predicate String)
          `shouldBe` "contains /foo/i"
        show (isEmpty :: Predicate [()]) `shouldBe` "empty"
        show (nonEmpty :: Predicate [()]) `shouldBe` "non-empty"
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
        show (containsKey (eq "foo") :: Predicate [(String, String)])
          `shouldBe` "contains key \"foo\""
        show
          ( containsEntry (eq "foo") (eq "bar") ::
              Predicate [(String, String)]
          )
          `shouldBe` "contains entry (\"foo\",\"bar\")"
        show (keysAre [eq 1, eq 2, eq 3] :: Predicate [(Int, String)])
          `shouldBe` "keys are [1,2,3]"
        show
          ( entriesAre [(eq 1, eq "one"), (eq 2, eq "two")] ::
              Predicate [(Int, String)]
          )
          `shouldBe` "entries are [(1,\"one\"),(2,\"two\")]"
        show (approxEq 1.0 :: Predicate Double) `shouldBe` "≈ 1.0"
        show (finite :: Predicate Double) `shouldBe` "finite"
        show (infinite :: Predicate Double) `shouldBe` "infinite"
        show (nAn :: Predicate Double) `shouldBe` "NaN"
        show (is even :: Predicate Int)
          `shouldSatisfy` ("custom predicate at " `isPrefixOf`)
        show ($(qIs [|even|]) :: Predicate Int) `shouldBe` "even"
        show (with length (gt 5) :: Predicate String)
          `shouldSatisfy` ("property at " `isPrefixOf`)
        show ($(qWith [|length|]) (gt 5) :: Predicate String)
          `shouldBe` "length: > 5"

    it "matches patterns" $
      example $ do
        let p = $(qMatch [p|Just (Left _)|])
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
