{-# LANGUAGE FlexibleContexts #-}

module Test.HMock.Internal.Util where

import Data.MonoTraversable (Element)
import qualified Data.Sequences as Seq
import GHC.Stack (CallStack, getCallStack, prettySrcLoc)

newtype Loc = Loc (Maybe String) deriving (Eq, Ord)

getSrcLoc :: CallStack -> Loc
getSrcLoc stack = Loc $ case map snd (getCallStack stack) of
  (loc : _) -> Just (prettySrcLoc loc)
  _ -> Nothing

showWithLoc :: Loc -> String -> String
showWithLoc (Loc Nothing) s = s
showWithLoc (Loc (Just loc)) s = s ++ " at " ++ loc

choices :: [a] -> [(a, [a])]
choices [] = []
choices (x : xs) = (x, xs) : (fmap (x :) <$> choices xs)

isSubsequenceOf :: (Seq.IsSequence t, Eq (Element t)) => t -> t -> Bool
xs `isSubsequenceOf` ys = case Seq.uncons xs of
  Nothing -> True
  Just (x, xs') -> case Seq.uncons (snd (Seq.break (== x) ys)) of
    Nothing -> False
    Just (_, ys') -> xs' `isSubsequenceOf` ys'
