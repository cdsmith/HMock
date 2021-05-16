{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.HMock.Internal.Predicates where

import Data.Foldable (toList)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import Data.Typeable (Proxy (..), Typeable, cast, typeRep)
import GHC.Stack (HasCallStack, callStack)
import Language.Haskell.TH (ExpQ, PatQ, pprint)
import Language.Haskell.TH.Syntax (lift)
import Test.HMock.Internal.TH.Util (removeModNames)
import Test.HMock.Internal.Util (choices, getSrcLoc, showWithLoc)

-- | A predicate, which tests values and either accepts or rejects them.  This
-- is similar to @a -> 'Bool'@, but also has a 'Show' instance to describe what
-- it is checking.
--
-- Predicates are used to define which arguments a general matcher should
-- accept.
data Predicate a = Predicate
  { showPredicate :: String,
    accept :: a -> Bool
  }

instance Show (Predicate a) where show = showPredicate

-- | A 'Predicate' that accepts anything at all.
--
-- >>> accept anything "foo"
-- >>> accept anything undefined
-- True
-- True
anything :: Predicate a
anything =
  Predicate
    { showPredicate = "anything",
      accept = const True
    }

-- | A 'Predicate' that accepts only the given value.
--
-- >>> accept (eq "foo") "foo"
-- >>> accept (eq "foo") "bar"
-- True
-- False
eq :: (Show a, Eq a) => a -> Predicate a
eq x =
  Predicate
    { showPredicate = show x,
      accept = (== x)
    }

-- | A 'Predicate' that accepts anything but the given value.
--
-- >>> accept (neq "foo") "foo"
-- >>> accept (neq "foo") "bar"
-- False
-- True
neq :: (Show a, Eq a) => a -> Predicate a
neq x =
  Predicate
    { showPredicate = "≠ " ++ show x,
      accept = (/= x)
    }

-- | A 'Predicate' that accepts anything greater than the given value.
--
-- >>> accept (gt 5) 4
-- >>> accept (gt 5) 5
-- >>> accept (gt 5) 6
-- False
-- False
-- True
gt :: (Show a, Ord a) => a -> Predicate a
gt x =
  Predicate
    { showPredicate = "> " ++ show x,
      accept = (> x)
    }

-- | A 'Predicate' that accepts anything greater than or equal to the given
-- value.
--
-- >>> accept (geq 5) 4
-- >>> accept (geq 5) 5
-- >>> accept (geq 5) 6
-- False
-- True
-- True
geq :: (Show a, Ord a) => a -> Predicate a
geq x =
  Predicate
    { showPredicate = "≥ " ++ show x,
      accept = (>= x)
    }

-- | A 'Predicate' that accepts anything less than the given value.
--
-- >>> accept (lt 5) 4
-- >>> accept (lt 5) 5
-- >>> accept (lt 5) 6
-- True
-- False
-- False
lt :: (Show a, Ord a) => a -> Predicate a
lt x =
  Predicate
    { showPredicate = "< " ++ show x,
      accept = (< x)
    }

-- | A 'Predicate' that accepts anything less than or equal to the given value.
--
-- >>> accept (leq 5) 4
-- >>> accept (leq 5) 5
-- >>> accept (leq 5) 6
-- True
-- True
-- False
leq :: (Show a, Ord a) => a -> Predicate a
leq x =
  Predicate
    { showPredicate = "≤ " ++ show x,
      accept = (<= x)
    }

-- | A 'Predicate' that accepts 'Maybe' values of @'Just' x@, where @x@ matches
-- the given child 'Predicate'.
--
-- >>> accept (just (lt 5)) Nothing
-- >>> accept (just (lt 5)) (Just 10)
-- >>> accept (just (lt 5)) (Just 4)
-- False
-- False
-- True
just :: Predicate a -> Predicate (Maybe a)
just p =
  Predicate
    { showPredicate = "Just " ++ showPredicate p,
      accept = \case Nothing -> False; Just x -> accept p x
    }

-- | A 'Predicate' that accepts anything accepted by both of its children.
--
-- >>> accept (lt 5 `andP` gt 3) 3
-- >>> accept (lt 5 `andP` gt 3) 4
-- >>> accept (lt 5 `andP` gt 3) 5
-- False
-- True
-- False
andP :: Predicate a -> Predicate a -> Predicate a
p `andP` q =
  Predicate
    { showPredicate = showPredicate p ++ " and " ++ showPredicate q,
      accept = \x -> accept p x && accept q x
    }

-- | A 'Predicate' that accepts anything accepted by either of its children.
--
-- >>> accept (lt 4 `orP` gt 5) 3
-- >>> accept (lt 4 `orP` gt 5) 4
-- >>> accept (lt 4 `orP` gt 5) 6
-- True
-- False
-- True
orP :: Predicate a -> Predicate a -> Predicate a
p `orP` q =
  Predicate
    { showPredicate = showPredicate p ++ " or " ++ showPredicate q,
      accept = \x -> accept p x || accept q x
    }

-- | A 'Predicate' that inverts another 'Predicate', accepting whatever its
-- child rejects, and rejecting whatever its child accepts.
--
-- >>> accept (notP (eq 5)) 4
-- >>> accept (notP (eq 5)) 5
-- True
-- False
notP :: Predicate a -> Predicate a
notP p =
  Predicate
    { showPredicate = "not " ++ showPredicate p,
      accept = not . accept p
    }

-- | A 'Predicate' that accepts lists or 'String's that start with the given
-- prefix.
--
-- >>> accept (startsWith "foo") "football"
-- >>> accept (startsWith "foo") "soccer"
-- True
-- False
startsWith :: (Eq a, Show a) => [a] -> Predicate [a]
startsWith s =
  Predicate
    { showPredicate = "starts with " ++ show s,
      accept = (s `isPrefixOf`)
    }

-- | A 'Predicate' that accepts lists or 'String's that and with the given
-- suffix.
--
-- >>> accept (endsWith "ow") "crossbow"
-- >>> accept (endsWith "ow") "trebuchet"
-- True
-- False
endsWith :: (Eq a, Show a) => [a] -> Predicate [a]
endsWith s =
  Predicate
    { showPredicate = "ends with " ++ show s,
      accept = (s `isSuffixOf`)
    }

-- | A 'Predicate' that accepts lists or 'String's that contain the given
-- subsequence or substring.
--
-- >>> accept (hasSubstr "i") "team"
-- >>> accept (hasSubstr "i") "partnership"
-- False
-- True
hasSubstr :: (Eq a, Show a) => [a] -> Predicate [a]
hasSubstr s =
  Predicate
    { showPredicate = "has substring " ++ show s,
      accept = (s `isInfixOf`)
    }

-- | A 'Predicate' that accepts lists or other 'Foldable' structures whose
-- number of elements match the child 'Predicate'.
--
-- >>> accept (sizeIs (lt 3)) ['a' .. 'f']
-- >>> accept (sizeIs (lt 3)) ['a' .. 'b']
-- False
-- True
sizeIs :: Foldable t => Predicate Int -> Predicate (t a)
sizeIs p =
  Predicate
    { showPredicate = "size " ++ showPredicate p,
      accept = accept p . length
    }

-- | A 'Predicate' that accepts lists or other 'Foldable' structures of a fixed
-- size, whose contents match the corresponding 'Predicate' in the given list.
--
-- >>> accept (elemsAre [lt 3, gt 4, lt 5]) []
-- >>> accept (elemsAre [lt 3, gt 4, lt 5]) [2, 3, 4]
-- >>> accept (elemsAre [lt 3, gt 4, lt 5]) [2, 10, 1]
-- False
-- False
-- True
elemsAre :: Foldable t => [Predicate a] -> Predicate (t a)
elemsAre ps =
  Predicate
    { showPredicate = "[" ++ intercalate ", " (map showPredicate ps) ++ "]",
      accept = \xs ->
        length xs == length ps
          && and (zipWith accept ps (toList xs))
    }

-- | A 'Predicate' that accepts lists or other 'Foldable' structures of a fixed
-- size, whose contents match the corresponding 'Predicate' in the given list
-- in any order.
--
-- >>> accept (unorderedElemsAre [lt 4, gt 10]) [2, 11]
-- >>> accept (unorderedElemsAre [lt 4, gt 10]) [11, 2]
-- >>> accept (unorderedElemsAre [lt 4, gt 10]) [2, 2]
-- >>> accept (unorderedElemsAre [lt 4, gt 10]) [2]
-- True
-- True
-- False
-- False
unorderedElemsAre :: Foldable t => [Predicate a] -> Predicate (t a)
unorderedElemsAre ps =
  Predicate
    { showPredicate =
        "(any order) ["
          ++ intercalate ", " (map showPredicate ps)
          ++ "]",
      accept = \xs -> length xs == length ps && matches ps (toList xs)
    }
  where
    matches (q : qs) xs = or [matches qs ys | (y, ys) <- choices xs, accept q y]
    matches [] _ = True

-- | A 'Predicate' that accepts lists or other 'Foldable' structures whose
-- elements all match the child 'Predicate'.
--
-- >>> accept (allElemsAre (gt 5)) [4, 5, 6]
-- >>> accept (allElemsAre (gt 5)) [6, 7, 8]
-- >>> accept (allElemsAre (gt 5)) []
-- False
-- True
-- True
allElemsAre :: Foldable t => Predicate a -> Predicate (t a)
allElemsAre p =
  Predicate
    { showPredicate = "all " ++ showPredicate p,
      accept = all (accept p)
    }

-- | A 'Predicate' that accepts lists or other 'Foldable' structures which
-- contain at least one element matching the child 'Predicate'.
--
-- >>> accept (anyElemIs (gt 5)) [3, 4, 5]
-- >>> accept (anyElemIs (gt 5)) [4, 5, 6]
-- >>> accept (anyElemIs (gt 5)) []
-- False
-- True
-- False
anyElemIs :: Foldable t => Predicate a -> Predicate (t a)
anyElemIs p =
  Predicate
    { showPredicate = "any " ++ showPredicate p,
      accept = any (accept p)
    }

-- | A conversion from @a -> 'Bool'@ to 'Predicate'.  This is a fallback that
-- can be used to build a 'Predicate' that checks anything at all.  However, its
-- description will be less helpful than standard 'Predicate's.
--
-- >>> accept (suchThat even) 3
-- >>> accept (suchThat even) 4
-- False
-- True
suchThat :: HasCallStack => (a -> Bool) -> Predicate a
suchThat f =
  Predicate
    { showPredicate = showWithLoc (getSrcLoc callStack) "custom predicate",
      accept = f
    }

-- | A Template Haskell splice that turns a quoted pattern into a predicate that
-- accepts values that match the pattern.
--
-- >>> accept $(match [p| Just (Left _) |]) Nothing
-- >>> accept $(match [p| Just (Left _) |]) (Just (Left 5))
-- >>> accept $(match [p| Just (Left _) |]) (Just (Right 5))
-- False
-- True
-- False
match :: PatQ -> ExpQ
match qpat =
  [|
    Predicate
      { showPredicate = $(lift . pprint . removeModNames =<< qpat),
        accept = \case
          $(qpat) -> True
          _ -> False
      }
    |]

-- | Converts a 'Predicate' to a new type.  Typically used with visible type
-- application, as in @'typed' @Int ('lt' 42)@.  This will only match if the
-- argument is an Int, and also less than 42.
--
-- >>> accept (typed @String anything) "foo"
-- >>> accept (typed @String (sizeIs (gt 5))) "foo"
-- >>> accept (typed @String anything) (42 :: Int)
-- True
-- False
-- False
typed :: forall a b. (Typeable a, Typeable b) => Predicate a -> Predicate b
typed p =
  Predicate
    { showPredicate =
        showPredicate p ++ " :: " ++ show (typeRep (Proxy :: Proxy a)),
      accept = \x -> case cast x of
        Nothing -> False
        Just y -> accept p y
    }
