{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Internal utilities used for HMock implementation.
module Test.HMock.Internal.Util where

import GHC.Stack (CallStack, getCallStack, prettySrcLoc)

-- | Identifies one instance of a mockable interface, for front ends that can
-- have several instances of the same interface in one test.  Rules and calls
-- carry a @'Maybe' t'InstanceId'@: 'Nothing' means no particular instance,
-- which is what a front end uses throughout if its interfaces have no
-- instances to tell apart.
--
-- Identity is the number, which is unique within a test.  The name is how the
-- instance appears in messages; it is also unique within a test, and defaults
-- to the number when the front end doesn't supply one.
data InstanceId = InstanceId
  { instanceNumber :: Int,
    instanceName :: String
  }

instance Eq InstanceId where
  a == b = instanceNumber a == instanceNumber b

instance Ord InstanceId where
  compare a b = compare (instanceNumber a) (instanceNumber b)

instance Show InstanceId where
  show = instanceName

-- | Describes which instance a rule or call belongs to, for use in error
-- messages.  Empty when no particular instance is meant.
showInstanceId :: Maybe InstanceId -> String
showInstanceId Nothing = ""
showInstanceId (Just c) = " on instance " ++ show c

-- | Whether a rule for the first instance applies to a call from the second.
-- A rule with no instance applies to every call; a rule for a particular
-- instance applies only to calls from that instance.
appliesTo :: Maybe InstanceId -> Maybe InstanceId -> Bool
appliesTo Nothing _ = True
appliesTo (Just c) call = call == Just c

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
