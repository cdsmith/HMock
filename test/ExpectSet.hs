{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExpectSet where

import Control.Arrow (second)
import Data.List (foldl')
import Test.HMock.Internal.ExpectSet
import Test.HMock.Multiplicity
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

instance Arbitrary (ExpectSet Int) where
  arbitrary = do
    n <- getSize
    frequency
      [ (1, return ExpectNothing),
        (5, ExpectStep <$> choose (1, 100)),
        (n, scale (`div` 2) $ ExpectSequence <$> arbitrary <*> arbitrary),
        (n, scale (`div` 2) $ ExpectInterleave <$> arbitrary <*> arbitrary),
        (n, scale (`div` 2) $ ExpectEither <$> arbitrary <*> arbitrary),
        (n, scale (subtract 1) $ ExpectMulti <$> arbitrary <*> arbitrary),
        (n, scale (subtract 1) $ ExpectConsecutive <$> arbitrary <*> arbitrary)
      ]

  shrink ExpectNothing = []
  shrink (ExpectStep _) = [ExpectNothing]
  shrink (ExpectSequence e f) =
    [ExpectSequence e' f | e' <- shrink e]
      ++ [ExpectSequence e f' | f' <- shrink f]
      ++ [e, f]
  shrink (ExpectInterleave e f) =
    [ExpectSequence e' f | e' <- shrink e]
      ++ [ExpectSequence e f' | f' <- shrink f]
      ++ [e, f]
  shrink (ExpectEither e f) =
    [ExpectSequence e' f | e' <- shrink e]
      ++ [ExpectSequence e f' | f' <- shrink f]
      ++ [e, f]
  shrink (ExpectMulti mult e) =
    [ExpectMulti mult' e | mult' <- shrink mult]
      ++ [ExpectMulti mult e' | e' <- shrink e]
      ++ [e]
  shrink (ExpectConsecutive mult e) =
    [ExpectConsecutive mult' e | mult' <- shrink mult]
      ++ [ExpectConsecutive mult e' | e' <- shrink e]
      ++ [e]

instance Arbitrary Multiplicity where
  arbitrary =
    between
      <$> (fromInteger <$> arbitrary)
      <*> (fromInteger . getNonNegative <$> arbitrary)

liveSteps' :: ExpectSet step -> [(step, ExpectSet step)]
liveSteps' = map (second simplify) . liveSteps

expectSetSpec :: SpecWith ()
expectSetSpec = modifyMaxSuccess (const 1000) $ do
  describe "ExpectSet" $ do
    describe "satisfied" $ do
      it "agrees with excess" $
        property $
          \(es :: ExpectSet Int) ->
            if satisfied es
              then excess es === ExpectNothing
              else excess es =/= ExpectNothing

    let sameBehavior ::
          (Show a, Eq a) => Int -> ExpectSet a -> ExpectSet a -> Property
        sameBehavior 0 e f = satisfied e === satisfied f
        sameBehavior d e f =
          satisfied e === satisfied f
            .&&. foldl' (.&&.) (property True) (zipWith (===) esteps fsteps)
            .&&. foldl'
              (.&&.)
              (property True)
              (zipWith (sameBehavior (d - 1)) econts fconts)
          where
            (esteps, econts) = unzip (liveSteps e)
            (fsteps, fconts) = unzip (liveSteps f)

    describe "simplify" $ do
      it "always terminates" $
        property $
          \(es :: ExpectSet Int) ->
            -- Hack to force deep evaluation
            let es' = simplify es in formatExpectSet es' == formatExpectSet es'

      it "preserves behavior" $
        property $
          \(es :: ExpectSet Int) -> sameBehavior 3 es (simplify es)

    describe "liveSteps" $ do
      it "expects nothing" $
        example $ liveSteps' (ExpectNothing :: ExpectSet Int) `shouldBe` []

      it "expects a step" $
        example $
          liveSteps' (ExpectStep 42)
            `shouldBe` [(42 :: Int, ExpectNothing)]

      it "expects the first step in a sequence" $
        example $ do
          let input = ExpectSequence (ExpectStep 1) (ExpectStep 2)
          liveSteps' input `shouldBe` [(1 :: Int, ExpectStep 2)]

      it "skips an exhaustable first step in a sequence" $
        example $ do
          let input = ExpectSequence ExpectNothing (ExpectStep 2)
          liveSteps' input `shouldBe` [(2 :: Int, ExpectNothing)]

      it "skips an exhaustable first step in a sequence" $
        example $ do
          let input = ExpectSequence ExpectNothing (ExpectStep 2)
          liveSteps' input `shouldBe` [(2 :: Int, ExpectNothing)]

      it "expects either step in an interleave" $
        example $ do
          let input = ExpectInterleave (ExpectStep 1) (ExpectStep 2)
          liveSteps' input
            `shouldBe` [(1 :: Int, ExpectStep 2), (2, ExpectStep 1)]

      it "optional has same behavior as or-empty" $
        property $
          \(es :: ExpectSet Int) ->
            let a = ExpectEither ExpectNothing es
                b = ExpectMulti (atMost 1) es
             in sameBehavior 3 a b