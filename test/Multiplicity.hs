{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Multiplicity where

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
