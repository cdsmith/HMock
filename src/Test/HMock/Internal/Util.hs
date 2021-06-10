{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.HMock.Internal.Util where

import Data.MonoTraversable (Element)
import qualified Data.Sequences as Seq
import GHC.Stack (CallStack, getCallStack, prettySrcLoc)

data Located a = Loc (Maybe String) a deriving (Eq, Ord, Functor)

locate :: CallStack -> a -> Located a
locate stack = case map snd (getCallStack stack) of
  (loc : _) -> Loc (Just (prettySrcLoc loc))
  _ -> Loc Nothing

withLoc :: Located String -> String
withLoc (Loc Nothing s) = s
withLoc (Loc (Just loc) s) = s ++ " at " ++ loc

choices :: [a] -> [(a, [a])]
choices [] = []
choices (x : xs) = (x, xs) : (fmap (x :) <$> choices xs)

isSubsequenceOf :: (Seq.IsSequence t, Eq (Element t)) => t -> t -> Bool
xs `isSubsequenceOf` ys = case Seq.uncons xs of
  Nothing -> True
  Just (x, xs') -> case Seq.uncons (snd (Seq.break (== x) ys)) of
    Nothing -> False
    Just (_, ys') -> xs' `isSubsequenceOf` ys'
