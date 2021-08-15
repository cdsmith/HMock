{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An implementation of bipartite matching using the Ford-Fulkerson algorithm.
module Test.HMock.Internal.FlowMatcher where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Array.IArray (Array, assocs, elems)
import Data.Array.ST
import Data.List ((\\))
import Data.Maybe (catMaybes)

-- $setup
-- >>> :set -Wno-type-defaults

-- | Computes the best bipartite matching of the elements in the two lists,
-- given the compatibility function.
--
-- Returns matched pairs, then unmatched lhs elements, then unmatched rhs
-- elements.
--
-- >>> bipartiteMatching (==) [1 .. 5] [6, 5 .. 2]
-- ([(2,2),(3,3),(4,4),(5,5)],[1],[6])
bipartiteMatching ::
  forall a b. (a -> b -> Bool) -> [a] -> [b] -> ([(a, b)], [a], [b])
bipartiteMatching compatible xs ys = (matchedPairs, unmatchedX, unmatchedY)
  where
    matchedPairs :: [(a, b)]
    matchedPairs = [(xs !! i, ys !! j) | (i, Just j) <- assocs matches]

    unmatchedX :: [a]
    unmatchedX = [xs !! i | (i, Nothing) <- assocs matches]

    unmatchedY :: [b]
    unmatchedY = [ys !! j | j <- [0 .. numYs - 1] \\ catMaybes (elems matches)]

    matches :: Array Int (Maybe Int)
    matches = runSTArray st

    st :: forall s. ST s (STArray s Int (Maybe Int))
    st = do
      compatArray <-
        newListArray
          ((0, 0), (numXs - 1, numYs - 1))
          [compatible x y | x <- xs, y <- ys] ::
          ST s (STArray s (Int, Int) Bool)
      matchArray <-
        newArray (0, numXs - 1) Nothing ::
          ST s (STArray s Int (Maybe Int))
      forM_ [0 .. numYs - 1] $ \j -> do
        seen <-
          newArray (0, numXs - 1) False :: ST s (STArray s Int Bool)
        _ <- go compatArray j matchArray seen
        return ()

      return matchArray

    numXs, numYs :: Int
    numXs = length xs
    numYs = length ys

    go ::
      forall s.
      STArray s (Int, Int) Bool ->
      Int ->
      STArray s Int (Maybe Int) ->
      STArray s Int Bool ->
      ST s Bool
    go compatArray j matchArray seen = loop False 0
      where
        loop True _ = return True
        loop _ i
          | i == numXs = return False
          | otherwise = do
            compat <- readArray compatArray (i, j)
            isSeen <- readArray seen i
            replace <-
              if isSeen || not compat
                then return False
                else do
                  writeArray seen i True
                  matchNum <- readArray matchArray i
                  case matchNum of
                    Nothing -> return True
                    Just n -> go compatArray n matchArray seen
            when replace $ writeArray matchArray i (Just j)
            loop replace (i + 1)
