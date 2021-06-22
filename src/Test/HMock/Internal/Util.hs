{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Internal utilities used for HMock implementation.
module Test.HMock.Internal.Util where

import Data.MonoTraversable (Element)
import qualified Data.Sequences as Seq
import GHC.Stack (CallStack, getCallStack, prettySrcLoc)

-- | A value together with its source location.
data Located a = Loc (Maybe String) a deriving (Eq, Ord, Functor)

-- | Annotates a value with its source location from the call stack.
locate :: CallStack -> a -> Located a
locate stack = case map snd (getCallStack stack) of
  (loc : _) -> Loc (Just (prettySrcLoc loc))
  _ -> Loc Nothing

-- | Formats a 'Located' 'String' to include its source location.
withLoc :: Located String -> String
withLoc (Loc Nothing s) = s
withLoc (Loc (Just loc) s) = s ++ " at " ++ loc

-- | Returns all ways to choose one element from a list, and the corresponding
-- remaining list.
choices :: [a] -> [(a, [a])]
choices [] = []
choices (x : xs) = (x, xs) : (fmap (x :) <$> choices xs)

-- | Checks if one sequence is a subsequence of another.
isSubsequenceOf :: (Seq.IsSequence t, Eq (Element t)) => t -> t -> Bool
xs `isSubsequenceOf` ys = case Seq.uncons xs of
  Nothing -> True
  Just (x, xs') -> case Seq.uncons (snd (Seq.break (== x) ys)) of
    Nothing -> False
    Just (_, ys') -> xs' `isSubsequenceOf` ys'
