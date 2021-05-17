{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.HMock.Internal.Predicates where

import Data.Char (toUpper)
import Data.MonoTraversable
import qualified Data.Sequences as Seq
import Data.Typeable (Proxy (..), Typeable, cast, typeRep)
import GHC.Stack (HasCallStack, callStack)
import Language.Haskell.TH (ExpQ, PatQ, pprint)
import Language.Haskell.TH.Syntax (lift)
import Test.HMock.Internal.TH.Util (removeModNames)
import Test.HMock.Internal.Util (choices, getSrcLoc, showWithLoc)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XTypeApplications

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
-- True
-- >>> accept anything undefined
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
-- True
-- >>> accept (eq "foo") "bar"
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
-- False
-- >>> accept (neq "foo") "bar"
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
-- False
-- >>> accept (gt 5) 5
-- False
-- >>> accept (gt 5) 6
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
-- False
-- >>> accept (geq 5) 5
-- True
-- >>> accept (geq 5) 6
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
-- True
-- >>> accept (lt 5) 5
-- False
-- >>> accept (lt 5) 6
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
-- True
-- >>> accept (leq 5) 5
-- True
-- >>> accept (leq 5) 6
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
-- >>> accept (just (eq "value")) Nothing
-- False
-- >>> accept (just (eq "value")) (Just "value")
-- True
-- >>> accept (just (eq "value")) (Just "wrong value")
-- False
just :: Predicate a -> Predicate (Maybe a)
just p =
  Predicate
    { showPredicate = "Just (" ++ showPredicate p ++ ")",
      accept = \case Just x -> accept p x; _ -> False
    }

-- | A 'Predicate' that accepts an 'Either' value of @'Left' x@, where @x@
-- matches the given child 'Predicate'.
--
-- >>> accept (left (eq "value")) (Left "value")
-- True
-- >>> accept (left (eq "value")) (Right "value")
-- False
-- >>> accept (left (eq "value")) (Left "wrong value")
-- False
left :: Predicate a -> Predicate (Either a b)
left p =
  Predicate
    { showPredicate = "Left (" ++ showPredicate p ++ ")",
      accept = \case Left x -> accept p x; _ -> False
    }

-- | A 'Predicate' that accepts an 'Either' value of @'Right' x@, where @x@
-- matches the given child 'Predicate'.
--
-- >>> accept (right (eq "value")) (Right "value")
-- True
-- >>> accept (right (eq "value")) (Right "wrong value")
-- False
-- >>> accept (right (eq "value")) (Left "value")
-- False
right :: Predicate b -> Predicate (Either a b)
right p =
  Predicate
    { showPredicate = "Right (" ++ showPredicate p ++ ")",
      accept = \case Right x -> accept p x; _ -> False
    }

-- | A 'Predicate' that accepts anything accepted by both of its children.
--
-- >>> accept (lt "foo" `andP` gt "bar") "eta"
-- True
-- >>> accept (lt "foo" `andP` gt "bar") "quz"
-- False
-- >>> accept (lt "foo" `andP` gt "bar") "alpha"
-- False
andP :: Predicate a -> Predicate a -> Predicate a
p `andP` q =
  Predicate
    { showPredicate = showPredicate p ++ " and " ++ showPredicate q,
      accept = \x -> accept p x && accept q x
    }

-- | A 'Predicate' that accepts anything accepted by either of its children.
--
-- >>> accept (lt "bar" `orP` gt "foo") "eta"
-- False
-- >>> accept (lt "bar" `orP` gt "foo") "quz"
-- True
-- >>> accept (lt "bar" `orP` gt "foo") "alpha"
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
-- >>> accept (notP (eq "negative")) "positive"
-- True
-- >>> accept (notP (eq "negative")) "negative"
-- False
notP :: Predicate a -> Predicate a
notP p =
  Predicate
    { showPredicate = "not " ++ showPredicate p,
      accept = not . accept p
    }

-- | A 'Predicate' that accepts sequences that start with the given prefix.
--
-- >>> accept (startsWith "fun") "fungible"
-- True
-- >>> accept (startsWith "gib") "fungible"
-- False
startsWith :: (Show t, Seq.IsSequence t, Eq (Element t)) => t -> Predicate t
startsWith pfx =
  Predicate
    { showPredicate = "starts with " ++ show pfx,
      accept = (pfx `Seq.isPrefixOf`)
    }

-- | A 'Predicate' that accepts sequences that end with the given suffix.
--
-- >>> accept (endsWith "ow") "crossbow"
-- True
-- >>> accept (endsWith "ow") "trebuchet"
-- False
endsWith :: (Show t, Seq.IsSequence t, Eq (Element t)) => t -> Predicate t
endsWith sfx =
  Predicate
    { showPredicate = "ends with " ++ show sfx,
      accept = (sfx `Seq.isSuffixOf`)
    }

-- | A 'Predicate' that accepts sequences that contain the given (consecutive)
-- substring.
--
-- >>> accept (hasSubstr "i") "team"
-- False
-- >>> accept (hasSubstr "i") "partnership"
-- True
hasSubstr :: (Show t, Seq.IsSequence t, Eq (Element t)) => t -> Predicate t
hasSubstr s =
  Predicate
    { showPredicate = "has substring " ++ show s,
      accept = (s `Seq.isInfixOf`)
    }

-- | A 'Predicate' that accepts sequences that contain the given (not
-- necessarily consecutive) subsequence.
--
-- >>> accept (hasSubsequence [1..5]) [1, 2, 3, 4, 5]
-- True
-- >>> accept (hasSubsequence [1..5]) [0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0]
-- True
-- >>> accept (hasSubsequence [1..5]) [2, 3, 5, 7, 11]
-- False
hasSubsequence :: (Show t, Seq.IsSequence t, Eq (Element t)) => t -> Predicate t
hasSubsequence s =
  Predicate
    { showPredicate = "has subsequence " ++ show s,
      accept = (s `isSubsequenceOf`)
    }
  where
    isSubsequenceOf :: (Seq.IsSequence t, Eq (Element t)) => t -> t -> Bool
    xs `isSubsequenceOf` ys = case Seq.uncons xs of
      Nothing -> True
      Just (x, xs') -> case Seq.uncons (snd (Seq.break (== x) ys)) of
        Nothing -> False
        Just (_, ys') -> xs' `isSubsequenceOf` ys'

-- | Transforms a 'Predicate' on 'String's or string-like types to match without
-- regard to case.
--
-- >>> accept (caseInsensitive startsWith "foo") "FOOTBALL!"
-- True
-- >>> accept (caseInsensitive endsWith "ball") "soccer"
-- False
-- >>> accept (caseInsensitive eq "time") "TIME"
-- True
-- >>> accept (caseInsensitive gt "NOTHING") "everything"
-- False
caseInsensitive ::
  (MonoTraversable t, Element t ~ Char) =>
  (t -> Predicate t) ->
  (t -> Predicate t)
caseInsensitive p s =
  Predicate
    { showPredicate = "(case insensitive) " ++ show (p s),
      accept = accept capP . omap toUpper
    }
  where
    capP = p (omap toUpper s)

-- | A 'Predicate' that accepts empty lists or other foldable structures.
--
-- >>> accept isEmpty []
-- True
-- >>> accept isEmpty [1, 2, 3]
-- False
-- >>> accept isEmpty ""
-- True
-- >>> accept isEmpty "gas tank"
-- False
isEmpty :: MonoFoldable t => Predicate t
isEmpty =
  Predicate
    { showPredicate = "empty",
      accept = onull
    }

-- | A 'Predicate' that accepts non-empty lists or other foldable structures.
--
-- >>> accept nonEmpty []
-- False
-- >>> accept nonEmpty [1, 2, 3]
-- True
-- >>> accept nonEmpty ""
-- False
-- >>> accept nonEmpty "gas tank"
-- True
nonEmpty :: MonoFoldable t => Predicate t
nonEmpty =
  Predicate
    { showPredicate = "nonempty",
      accept = not . onull
    }

-- | A 'Predicate' that accepts lists or other 'Foldable' structures whose
-- number of elements match the child 'Predicate'.
--
-- >>> accept (sizeIs (lt 3)) ['a' .. 'f']
-- False
-- >>> accept (sizeIs (lt 3)) ['a' .. 'b']
-- True
sizeIs :: MonoFoldable t => Predicate Int -> Predicate t
sizeIs p =
  Predicate
    { showPredicate = "size " ++ showPredicate p,
      accept = accept p . olength
    }

-- | A 'Predicate' that accepts lists or other 'Foldable' structures of a fixed
-- size, whose contents match the corresponding 'Predicate' in the given list.
--
-- >>> accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4]
-- True
-- >>> accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4, 5]
-- False
-- >>> accept (elemsAre [lt 3, lt 4, lt 5]) [2, 10, 4]
-- False
elemsAre :: MonoFoldable t => [Predicate (Element t)] -> Predicate t
elemsAre ps =
  Predicate
    { showPredicate = show ps,
      accept = \xs ->
        olength xs == olength ps
          && and (zipWith accept ps (otoList xs))
    }

-- | A 'Predicate' that accepts lists or other 'Foldable' structures of a fixed
-- size, whose contents match the corresponding 'Predicate' in the given list
-- in any order.
--
-- >>> accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3]
-- True
-- >>> accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [2, 3, 1]
-- True
-- >>> accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3, 4]
-- False
-- >>> accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 3]
-- False
unorderedElemsAre :: MonoFoldable t => [Predicate (Element t)] -> Predicate t
unorderedElemsAre ps =
  Predicate
    { showPredicate =
        "(any order) " ++ show ps,
      accept = matches ps . otoList
    }
  where
    matches (q : qs) xs = or [matches qs ys | (y, ys) <- choices xs, accept q y]
    matches [] xs = null xs

-- | A 'Predicate' that accepts lists or other 'Foldable' structures whose
-- elements all match the child 'Predicate'.
--
-- >>> accept (each (gt 5)) [4, 5, 6]
-- False
-- >>> accept (each (gt 5)) [6, 7, 8]
-- True
-- >>> accept (each (gt 5)) []
-- True
each :: MonoFoldable t => Predicate (Element t) -> Predicate t
each p =
  Predicate
    { showPredicate = "each (" ++ showPredicate p ++ ")",
      accept = oall (accept p)
    }

-- | A 'Predicate' that accepts data structures which contain at least one
-- element matching the child 'Predicate'.
--
-- >>> accept (contains (gt 5)) [3, 4, 5]
-- False
-- >>> accept (contains (gt 5)) [4, 5, 6]
-- True
-- >>> accept (contains (gt 5)) []
-- False
contains :: MonoFoldable t => Predicate (Element t) -> Predicate t
contains p =
  Predicate
    { showPredicate = "contains (" ++ showPredicate p ++ ")",
      accept = oany (accept p)
    }

-- | A 'Predicate' that accepts date structures which contain an element
-- satisfying each of the child predicates.  @'containsAll' [p1, p2, ..., pn]@
-- is equivalent to @'contains' p1 `'andP'` 'contains' p2 `'andP'` ... `'andP'`
-- 'contains' pn@.
--
-- >>> accept (containsAll [eq "foo", eq "bar"]) ["bar", "foo"]
-- True
-- >>> accept (containsAll [eq "foo", eq "bar"]) ["foo"]
-- False
-- >>> accept (containsAll [eq "foo", eq "bar"]) ["foo", "bar", "qux"]
-- True
containsAll :: MonoFoldable t => [Predicate (Element t)] -> Predicate t
containsAll ps =
  Predicate
    { showPredicate = "contains all of " ++ show ps,
      accept = \xs -> all (flip oany xs . accept) ps
    }

-- | A 'Predicate' that accepts date structures whose elements all satisfy at
-- least one of the child predicates.  @'containsOnly' [p1, p2, ..., pn]@ is
-- equivalent to @'each' (p1 `'orP'` p2 `'orP'` ... `'orP'` pn)@.
--
-- >>> accept (containsOnly [eq "foo", eq "bar"]) ["foo", "foo"]
-- True
-- >>> accept (containsOnly [eq "foo", eq "bar"]) ["foo", "bar"]
-- True
-- >>> accept (containsOnly [eq "foo", eq "bar"]) ["foo", "qux"]
-- False
containsOnly :: MonoFoldable t => [Predicate (Element t)] -> Predicate t
containsOnly ps =
  Predicate
    { showPredicate = "contains only " ++ show ps,
      accept = oall (\x -> any (`accept` x) ps)
    }

-- | A 'Predicate' that accepts values of 'RealFloat' types that are close to
-- the given number.  The expected precision is scaled based on the target
-- value, so that reasonable rounding error is accepted but grossly inaccurate
-- results are not.
--
-- The following naive use of 'eq' fails due to rounding:
--
-- >>> accept (eq 1.0) (sum (replicate 100 0.01))
-- False
--
-- The solution is to use 'approxEq', which accounts for rounding error.
-- However, 'approxEq' doesn't accept results that are far enough off that they
-- likely arise from incorrect calculations instead of rounding error.
--
-- >>> accept (approxEq 1.0) (sum (replicate 100 0.01))
-- True
-- >>> accept (approxEq 1.0) (sum (replicate 100 0.009999))
-- False
approxEq :: (Show a, RealFloat a) => a -> Predicate a
approxEq x =
  Predicate
    { showPredicate = "≈ " ++ show x,
      accept = \y -> abs (x - y) < diff
    }
  where
    diff = encodeFloat 1 (snd (decodeFloat x) + floatDigits x `div` 2)

-- | A 'Predicate' that accepts finite numbers of any 'RealFloat' type.
--
-- >>> accept finite 1.0
-- True
-- >>> accept finite (0 / 0)
-- False
-- >>> accept finite (1 / 0)
-- False
finite :: RealFloat a => Predicate a
finite =
  Predicate
    { showPredicate = "finite",
      accept = \x -> not (isInfinite x) && not (isNaN x)
    }

-- | A 'Predicate' that accepts infinite numbers of any 'RealFloat' type.
--
-- >>> accept infinite 1.0
-- False
-- >>> accept infinite (0 / 0)
-- False
-- >>> accept infinite (1 / 0)
-- True
infinite :: RealFloat a => Predicate a
infinite =
  Predicate
    { showPredicate = "infinite",
      accept = isInfinite
    }

-- | A 'Predicate' that accepts NaN values of any 'RealFloat' type.
--
-- >>> accept nAn 1.0
-- False
-- >>> accept nAn (0 / 0)
-- True
-- >>> accept nAn (1 / 0)
-- False
nAn :: RealFloat a => Predicate a
nAn =
  Predicate
    { showPredicate = "NaN",
      accept = isNaN
    }

-- | A conversion from @a -> 'Bool'@ to 'Predicate'.  This is a fallback that
-- can be used to build a 'Predicate' that checks anything at all.  However, its
-- description will be less helpful than standard 'Predicate's.
--
-- >>> accept (suchThat even) 3
-- False
-- >>> accept (suchThat even) 4
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
-- False
-- >>> accept $(match [p| Just (Left _) |]) (Just (Left 5))
-- True
-- >>> accept $(match [p| Just (Left _) |]) (Just (Right 5))
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
-- application, as in the examples below.
--
-- >>> accept (typed @String anything) "foo"
-- True
-- >>> accept (typed @String (sizeIs (gt 5))) "foo"
-- False
-- >>> accept (typed @String anything) (42 :: Int)
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
