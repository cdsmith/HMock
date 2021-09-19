{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Internal utilities used for HMock implementation.
module Test.HMock.Internal.Util where

import GHC.Stack (CallStack, getCallStack, prettySrcLoc)

-- | A value together with its source location.
data Located a = Loc (Maybe String) a deriving (Functor)

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
