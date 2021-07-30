{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines 'Predicate's which you can use to match the arguments of
-- a method in your execution plan.
module Test.HMock.Predicates
  ( Predicate (..),
    anything,
    eq,
    neq,
    gt,
    geq,
    lt,
    leq,
    just,
    nothing,
    left,
    right,
    zipP,
    zip3P,
    zip4P,
    zip5P,
    andP,
    orP,
    notP,
    startsWith,
    endsWith,
    hasSubstr,
    hasSubsequence,
    caseInsensitive,
    matchesRegex,
    matchesCaseInsensitiveRegex,
    containsRegex,
    containsCaseInsensitiveRegex,
    isEmpty,
    nonEmpty,
    sizeIs,
    elemsAre,
    unorderedElemsAre,
    each,
    contains,
    containsAll,
    containsOnly,
    keys,
    values,
    approxEq,
    positive,
    negative,
    nonPositive,
    nonNegative,
    finite,
    infinite,
    nAn,
    is,
    qIs,
    with,
    qWith,
    qMatch,
    typed,
  )
where

import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing)
import Data.MonoTraversable
import qualified Data.Sequences as Seq
import Data.Typeable (Proxy (..), Typeable, cast, typeRep)
import GHC.Exts (IsList (Item, toList))
import GHC.Stack (HasCallStack, callStack)
import Language.Haskell.TH (ExpQ, PatQ, pprint)
import Language.Haskell.TH.Syntax (lift)
import Test.HMock.Internal.TH (removeModNames)
import Test.HMock.Internal.Util (choices, isSubsequenceOf, locate, withLoc)
import Text.Regex.TDFA hiding (match)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XTypeApplications
-- >>> :set -Wno-type-defaults

-- | A predicate, which tests values and either accepts or rejects them.  This
-- is similar to @a -> 'Bool'@, but also has a 'Show' instance to describe what
-- it is checking.
--
-- 'Predicate's are used to define which arguments a general matcher should
-- accept.
data Predicate a = Predicate
  { showPredicate :: String,
    showNegation :: String,
    accept :: a -> Bool,
    explain :: a -> String
  }

instance Show (Predicate a) where show = showPredicate

withDefaultExplain ::
  (a -> String) -> String -> ((a -> String) -> Predicate a) -> Predicate a
withDefaultExplain format connector mk = p
  where
    p = mk $ \x ->
      if accept p x
        then format x ++ connector ++ showPredicate p
        else format x ++ connector ++ showNegation p

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
      showNegation = "nothing",
      accept = const True,
      explain = const "always matches"
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
      showNegation = "≠ " ++ show x,
      accept = (== x),
      explain = \y ->
        if y == x
          then show y ++ " = " ++ show x
          else show y ++ " ≠ " ++ show x
    }

-- | A 'Predicate' that accepts anything but the given value.
--
-- >>> accept (neq "foo") "foo"
-- False
-- >>> accept (neq "foo") "bar"
-- True
neq :: (Show a, Eq a) => a -> Predicate a
neq = notP . eq

-- | A 'Predicate' that accepts anything greater than the given value.
--
-- >>> accept (gt 5) 4
-- False
-- >>> accept (gt 5) 5
-- False
-- >>> accept (gt 5) 6
-- True
gt :: (Show a, Ord a) => a -> Predicate a
gt x = withDefaultExplain show " " $ \explainImpl ->
  Predicate
    { showPredicate = "> " ++ show x,
      showNegation = "≤ " ++ show x,
      accept = (> x),
      explain = explainImpl
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
geq x = withDefaultExplain show " " $ \explainImpl ->
  Predicate
    { showPredicate = "≥ " ++ show x,
      showNegation = "< " ++ show x,
      accept = (>= x),
      explain = explainImpl
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
lt = notP . geq

-- | A 'Predicate' that accepts anything less than or equal to the given value.
--
-- >>> accept (leq 5) 4
-- True
-- >>> accept (leq 5) 5
-- True
-- >>> accept (leq 5) 6
-- False
leq :: (Show a, Ord a) => a -> Predicate a
leq = notP . gt

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
      showNegation = "not Just (" ++ showPredicate p ++ ")",
      accept = \case Just x -> accept p x; _ -> False,
      explain = \case Just x -> explain p x; _ -> "Nothing ≠ Just _"
    }

-- | A Predicate that accepts 'Maybe' values of @'Nothing'@.  Unlike 'eq', this
-- doesn't require 'Eq' or 'Show' instances.
--
-- >>> accept nothing Nothing
-- True
--
-- >>> accept nothing (Just "something")
-- False
nothing :: Predicate (Maybe a)
nothing =
  Predicate
    { showPredicate = "Nothing",
      showNegation = "Just anything",
      accept = isNothing,
      explain = \case Nothing -> "Nothing = Nothing"; _ -> "Just _ ≠ Nothing"
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
      showNegation = "not Left (" ++ showPredicate p ++ ")",
      accept = \case Left x -> accept p x; _ -> False,
      explain = \case Left x -> explain p x; _ -> "Right _ ≠ Left _"
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
      showNegation = "not Right (" ++ showPredicate p ++ ")",
      accept = \case Right x -> accept p x; _ -> False,
      explain = \case Right x -> explain p x; _ -> "Left _ ≠ Right _"
    }

-- | A 'Predicate' that accepts pairs whose elements satisfy the corresponding
-- child 'Predicate's.
--
-- >>> accept (zipP (eq "foo") (eq "bar")) ("foo", "bar")
-- True
-- >>> accept (zipP (eq "foo") (eq "bar")) ("bar", "foo")
-- False
zipP :: Predicate a -> Predicate b -> Predicate (a, b)
zipP p1 p2 =
  Predicate
    { showPredicate = show (p1, p2),
      showNegation = "not " ++ show (p1, p2),
      accept = all fst . acceptAndExplain,
      explain = \xs ->
        let results = acceptAndExplain xs
            significant
              | all fst results = results
              | otherwise = filter (not . fst) results
         in intercalate " and " $ map snd significant
    }
  where
    acceptAndExplain = \(x1, x2) ->
      [ (accept p1 x1, explain p1 x1),
        (accept p2 x2, explain p2 x2)
      ]

-- | A 'Predicate' that accepts 3-tuples whose elements satisfy the
-- corresponding child 'Predicate's.
--
-- >>> accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("foo", "bar", "qux")
-- True
-- >>> accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("qux", "bar", "foo")
-- False
zip3P :: Predicate a -> Predicate b -> Predicate c -> Predicate (a, b, c)
zip3P p1 p2 p3 =
  Predicate
    { showPredicate = show (p1, p2, p3),
      showNegation = "not " ++ show (p1, p2, p3),
      accept = all fst . acceptAndExplain,
      explain = \xs ->
        let results = acceptAndExplain xs
            significant
              | all fst results = results
              | otherwise = filter (not . fst) results
         in intercalate " and " $ map snd significant
    }
  where
    acceptAndExplain = \(x1, x2, x3) ->
      [ (accept p1 x1, explain p1 x1),
        (accept p2 x2, explain p2 x2),
        (accept p3 x3, explain p3 x3)
      ]

-- | A 'Predicate' that accepts 3-tuples whose elements satisfy the
-- corresponding child 'Predicate's.
--
-- >>> accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (1, 2, 3, 4)
-- True
-- >>> accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (4, 3, 2, 1)
-- False
zip4P ::
  Predicate a ->
  Predicate b ->
  Predicate c ->
  Predicate d ->
  Predicate (a, b, c, d)
zip4P p1 p2 p3 p4 =
  Predicate
    { showPredicate = show (p1, p2, p3, p4),
      showNegation = "not " ++ show (p1, p2, p3, p4),
      accept = all fst . acceptAndExplain,
      explain = \xs ->
        let results = acceptAndExplain xs
            significant
              | all fst results = results
              | otherwise = filter (not . fst) results
         in intercalate " and " $ map snd significant
    }
  where
    acceptAndExplain = \(x1, x2, x3, x4) ->
      [ (accept p1 x1, explain p1 x1),
        (accept p2 x2, explain p2 x2),
        (accept p3 x3, explain p3 x3),
        (accept p4 x4, explain p4 x4)
      ]

-- | A 'Predicate' that accepts 3-tuples whose elements satisfy the
-- corresponding child 'Predicate's.
--
-- >>> accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (1, 2, 3, 4, 5)
-- True
-- >>> accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (5, 4, 3, 2, 1)
-- False
zip5P ::
  Predicate a ->
  Predicate b ->
  Predicate c ->
  Predicate d ->
  Predicate e ->
  Predicate (a, b, c, d, e)
zip5P p1 p2 p3 p4 p5 =
  Predicate
    { showPredicate = show (p1, p2, p3, p4, p5),
      showNegation = "not " ++ show (p1, p2, p3, p4, p5),
      accept = all fst . acceptAndExplain,
      explain = \xs ->
        let results = acceptAndExplain xs
            significant
              | all fst results = results
              | otherwise = filter (not . fst) results
         in intercalate " and " $ map snd significant
    }
  where
    acceptAndExplain = \(x1, x2, x3, x4, x5) ->
      [ (accept p1 x1, explain p1 x1),
        (accept p2 x2, explain p2 x2),
        (accept p3 x3, explain p3 x3),
        (accept p4 x4, explain p4 x4),
        (accept p5 x5, explain p5 x5)
      ]

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
      showNegation = showNegation p ++ " or " ++ showNegation q,
      accept = \x -> accept p x && accept q x,
      explain = \x ->
        if
            | not (accept p x) -> explain p x
            | not (accept q x) -> explain q x
            | otherwise -> explain p x ++ " and " ++ explain q x
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
p `orP` q = notP (notP p `andP` notP q)

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
    { showPredicate = showNegation p,
      showNegation = showPredicate p,
      accept = not . accept p,
      explain = explain p
    }

-- | A 'Predicate' that accepts sequences that start with the given prefix.
--
-- >>> accept (startsWith "fun") "fungible"
-- True
-- >>> accept (startsWith "gib") "fungible"
-- False
startsWith :: (Show t, Seq.IsSequence t, Eq (Element t)) => t -> Predicate t
startsWith pfx = withDefaultExplain show " " $ \explainImpl ->
  Predicate
    { showPredicate = "starts with " ++ show pfx,
      showNegation = "doesn't start with " ++ show pfx,
      accept = (pfx `Seq.isPrefixOf`),
      explain = explainImpl
    }

-- | A 'Predicate' that accepts sequences that end with the given suffix.
--
-- >>> accept (endsWith "ow") "crossbow"
-- True
-- >>> accept (endsWith "ow") "trebuchet"
-- False
endsWith :: (Show t, Seq.IsSequence t, Eq (Element t)) => t -> Predicate t
endsWith sfx = withDefaultExplain show " " $ \explainImpl ->
  Predicate
    { showPredicate = "ends with " ++ show sfx,
      showNegation = "doesn't end with " ++ show sfx,
      accept = (sfx `Seq.isSuffixOf`),
      explain = explainImpl
    }

-- | A 'Predicate' that accepts sequences that contain the given (consecutive)
-- substring.
--
-- >>> accept (hasSubstr "i") "team"
-- False
-- >>> accept (hasSubstr "i") "partnership"
-- True
hasSubstr :: (Show t, Seq.IsSequence t, Eq (Element t)) => t -> Predicate t
hasSubstr s = withDefaultExplain show " " $ \explainImpl ->
  Predicate
    { showPredicate = "has substring " ++ show s,
      showNegation = "doesn't have substring " ++ show s,
      accept = (s `Seq.isInfixOf`),
      explain = explainImpl
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
hasSubsequence s = withDefaultExplain show " " $ \explainImpl ->
  Predicate
    { showPredicate = "has subsequence " ++ show s,
      showNegation = "doesn't have subsequence " ++ show s,
      accept = (s `isSubsequenceOf`),
      explain = explainImpl
    }

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
  ( MonoFunctor t,
    MonoFunctor a,
    Element t ~ Char,
    Element a ~ Char
  ) =>
  (t -> Predicate a) ->
  (t -> Predicate a)
caseInsensitive p s =
  Predicate
    { showPredicate = "(case insensitive) " ++ show (p s),
      showNegation = "(case insensitive) " ++ show (notP (p s)),
      accept = accept capP . omap toUpper,
      explain = explain capP . omap toUpper
    }
  where
    capP = p (omap toUpper s)

-- | A 'Predicate' that accepts 'String's or string-like values matching a
-- regular expression.  The expression must match the entire argument.
--
-- You should not use @'caseInsensitive' 'matchesRegex'@, because regular
-- expression syntax itself is still case-sensitive even when the text you are
-- matching is not.  Instead, use 'matchesCaseInsensitiveRegex'.
--
-- >>> accept (matchesRegex "x{2,5}y?") "xxxy"
-- True
-- >>> accept (matchesRegex "x{2,5}y?") "xyy"
-- False
-- >>> accept (matchesRegex "x{2,5}y?") "wxxxyz"
-- False
matchesRegex :: (RegexLike Regex a, Eq a, Show a) => String -> Predicate a
matchesRegex s =
  Predicate
    { showPredicate = pat,
      showNegation = "not " ++ pat,
      accept = accepts,
      explain = \x ->
        if accepts x
          then show x ++ " matches " ++ pat
          else show x ++ " doesn't match " ++ pat
    }
  where
    pat = "/" ++ init (tail $ show s) ++ "/"
    accepts x = case matchOnceText r x of
      Just (a, _, b) -> a == empty && b == empty
      Nothing -> False
    r = makeRegexOpts comp exec s :: Regex
    comp = defaultCompOpt {newSyntax = True, lastStarGreedy = True}
    exec = defaultExecOpt {captureGroups = False}

-- | A 'Predicate' that accepts 'String's or string-like values matching a
-- regular expression in a case-insensitive way.  The expression must match the
-- entire argument.
--
-- You should use this instead of @'caseInsensitive' 'matchesRegex'@, because
-- regular expression syntax itself is still case-sensitive even when the text
-- you are matching is not.
--
-- >>> accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XXXY"
-- True
-- >>> accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XYY"
-- False
-- >>> accept (matchesCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ"
-- False
matchesCaseInsensitiveRegex ::
  (RegexLike Regex a, Eq a, Show a) => String -> Predicate a
matchesCaseInsensitiveRegex s =
  Predicate
    { showPredicate = pat,
      showNegation = "not " ++ pat,
      accept = accepts,
      explain = \x ->
        if accepts x
          then show x ++ " matches " ++ pat
          else show x ++ " doesn't match " ++ pat
    }
  where
    pat = "/" ++ init (tail $ show s) ++ "/i"
    accepts x = case matchOnceText r x of
      Just (a, _, b) -> a == empty && b == empty
      Nothing -> False
    r = makeRegexOpts comp exec s :: Regex
    comp =
      defaultCompOpt
        { newSyntax = True,
          lastStarGreedy = True,
          caseSensitive = False
        }
    exec = defaultExecOpt {captureGroups = False}

-- | A 'Predicate' that accepts 'String's or string-like values containing a
-- match for a regular expression.  The expression need not match the entire
-- argument.
--
-- You should not use @'caseInsensitive' 'containsRegex'@, because regular
-- expression syntax itself is still case-sensitive even when the text you are
-- matching is not.  Instead, use 'containsCaseInsensitiveRegex'.
--
-- >>> accept (containsRegex "x{2,5}y?") "xxxy"
-- True
-- >>> accept (containsRegex "x{2,5}y?") "xyy"
-- False
-- >>> accept (containsRegex "x{2,5}y?") "wxxxyz"
-- True
containsRegex :: (RegexLike Regex a, Eq a, Show a) => String -> Predicate a
containsRegex s = withDefaultExplain show " " $ \explainImpl ->
  Predicate
    { showPredicate = "contains " ++ pat,
      showNegation = "doesn't contain " ++ pat,
      accept = isJust . matchOnce r,
      explain = explainImpl
    }
  where
    pat = "/" ++ init (tail $ show s) ++ "/"
    r = makeRegexOpts comp exec s :: Regex
    comp = defaultCompOpt {newSyntax = True, lastStarGreedy = True}
    exec = defaultExecOpt {captureGroups = False}

-- | A 'Predicate' that accepts 'String's or string-like values containing a
-- match for a regular expression in a case-insensitive way.  The expression
-- need match the entire argument.
--
-- You should use this instead of @'caseInsensitive' 'containsRegex'@, because
-- regular expression syntax itself is still case-sensitive even when the text
-- you are matching is not.
--
-- >>> accept (containsCaseInsensitiveRegex "x{2,5}y?") "XXXY"
-- True
-- >>> accept (containsCaseInsensitiveRegex "x{2,5}y?") "XYY"
-- False
-- >>> accept (containsCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ"
-- True
containsCaseInsensitiveRegex ::
  (RegexLike Regex a, Eq a, Show a) => String -> Predicate a
containsCaseInsensitiveRegex s = withDefaultExplain show " " $ \explainImpl ->
  Predicate
    { showPredicate = "contains " ++ pat,
      showNegation = "doesn't contain " ++ pat,
      accept = isJust . matchOnce r,
      explain = explainImpl
    }
  where
    pat = "/" ++ init (tail $ show s) ++ "/i"
    r = makeRegexOpts comp exec s :: Regex
    comp =
      defaultCompOpt
        { newSyntax = True,
          lastStarGreedy = True,
          caseSensitive = False
        }
    exec = defaultExecOpt {captureGroups = False}

-- | A 'Predicate' that accepts empty data structures.
--
-- >>> accept isEmpty ([] :: [Int])
-- True
-- >>> accept isEmpty [1, 2, 3]
-- False
-- >>> accept isEmpty ""
-- True
-- >>> accept isEmpty "gas tank"
-- False
isEmpty :: (MonoFoldable t, Show t) => Predicate t
isEmpty = withDefaultExplain show " is " $ \explainImpl ->
  Predicate
    { showPredicate = "empty",
      showNegation = "non-empty",
      accept = onull,
      explain = explainImpl
    }

-- | A 'Predicate' that accepts non-empty data structures.
--
-- >>> accept nonEmpty ([] :: [Int])
-- False
-- >>> accept nonEmpty [1, 2, 3]
-- True
-- >>> accept nonEmpty ""
-- False
-- >>> accept nonEmpty "gas tank"
-- True
nonEmpty :: (MonoFoldable t, Show t) => Predicate t
nonEmpty = notP isEmpty

-- | A 'Predicate' that accepts data structures whose number of elements match
-- the child 'Predicate'.
--
-- >>> accept (sizeIs (lt 3)) ['a' .. 'f']
-- False
-- >>> accept (sizeIs (lt 3)) ['a' .. 'b']
-- True
sizeIs :: (MonoFoldable t, Show t) => Predicate Int -> Predicate t
sizeIs p =
  Predicate
    { showPredicate = "size " ++ showPredicate p,
      showNegation = "size " ++ showNegation p,
      accept = accept p . olength,
      explain = \y ->
        let detail
              | accept p (olength y) = showPredicate p
              | otherwise = showNegation p
            detailStr
              | show (olength y) == detail = ""
              | otherwise = ", which is " ++ detail
         in show y ++ " has size " ++ show (olength y) ++ detailStr
    }

-- | A 'Predicate' that accepts data structures whose contents each match the
-- corresponding 'Predicate' in the given list, in the same order.
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
      showNegation = "not " ++ show ps,
      accept = \xs ->
        olength xs == olength ps
          && and (zipWith accept ps (otoList xs)),
      explain = \xs ->
        let results = acceptAndExplain (otoList xs)
         in if
                | olength xs /= length ps ->
                  "wrong size (got "
                    ++ show (olength xs)
                    ++ "; expected "
                    ++ show (length ps)
                    ++ ")"
                | all fst results -> "elements are " ++ show ps
                | otherwise ->
                  intercalate "; and " $
                    snd <$> filter (not . fst) results
    }
  where
    acceptAndExplain xs = zipWith3 matchAndExplain [1 :: Int ..] ps xs
    matchAndExplain i p x =
      (accept p x, "in element #" ++ show i ++ ": " ++ explain p x)

-- | A 'Predicate' that accepts data structures whose contents each match the
-- corresponding 'Predicate' in the given list, in any order.
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
      showNegation =
        "not (in any order) " ++ show ps,
      accept = match ps . otoList,
      explain = \xs ->
        if
            | olength xs /= length ps ->
              "wrong size (got "
                ++ show (olength xs)
                ++ "; expected "
                ++ show (length ps)
                ++ ")"
            | match ps (otoList xs) -> "elements match"
            | otherwise -> "elements don't match"
    }
  where
    match (q : qs) xs = or [match qs ys | (y, ys) <- choices xs, accept q y]
    match [] xs = null xs

-- | A 'Predicate' that accepts data structures whose elements each match the
-- child 'Predicate'.
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
      showNegation = "contains (" ++ showNegation p ++ ")",
      accept = all fst . acceptAndExplain,
      explain = \xs ->
        let results = acceptAndExplain xs
            format (i, explanation) =
              "element #" ++ show i ++ ": " ++ explanation
         in if all fst results
              then "all elements " ++ showPredicate p
              else
                intercalate "; and " $
                  format . snd <$> filter (not . fst) results
    }
  where
    acceptAndExplain xs =
      [(accept p x, (i, explain p x)) | i <- [1 :: Int ..] | x <- otoList xs]

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
contains = notP . each . notP

-- | A 'Predicate' that accepts data structures which contain an element
-- satisfying each of the child 'Predicate's.  @'containsAll' [p1, p2, ..., pn]@
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
      showNegation = "not all of " ++ show ps,
      accept = \xs -> all (flip oany xs . accept) ps,
      explain = \xs ->
        if all (flip oany xs . accept) ps
          then "value contains all elements"
          else "value is missing elements"
    }

-- | A 'Predicate' that accepts data structures whose elements all satisfy at
-- least one of the child 'Predicate's.  @'containsOnly' [p1, p2, ..., pn]@ is
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
      showNegation = "not only " ++ show ps,
      accept = oall (\x -> any (`accept` x) ps),
      explain = \xs ->
        if oall (flip any ps . flip accept) xs
          then "value has no extra elements"
          else "value has extra elements"
    }

-- | Transforms a 'Predicate' on a list of keys into a 'Predicate' on map-like
-- data structures.
--
-- This is equivalent to @'with' ('map' 'fst' '.' 'toList')@, but more readable.
--
-- >>> accept (keys (each (eq "foo"))) [("foo", 5)]
-- True
--
-- >>> accept (keys (each (eq "foo"))) [("foo", 5), ("bar", 6)]
-- False
keys :: (IsList t, Item t ~ (k, v)) => Predicate [k] -> Predicate t
keys p =
  Predicate
    { showPredicate = "keys (" ++ showPredicate p ++ ")",
      showNegation = "keys (" ++ showNegation p ++ ")",
      accept = accept p . map fst . toList,
      explain = ("in keys, " ++) . explain p . map fst . toList
    }

-- | Transforms a 'Predicate' on a list of values into a 'Predicate' on map-like
-- data structures.
--
-- This is equivalent to @'with' ('map' 'snd' '.' 'toList')@, but more readable.
--
-- >>> accept (values (each (eq 5))) [("foo", 5), ("bar", 5)]
-- True
--
-- >>> accept (values (each (eq 5))) [("foo", 5), ("bar", 6)]
-- False
values :: (IsList t, Item t ~ (k, v)) => Predicate [v] -> Predicate t
values p =
  Predicate
    { showPredicate = "values (" ++ showPredicate p ++ ")",
      showNegation = "values (" ++ showNegation p ++ ")",
      accept = accept p . map snd . toList,
      explain = ("in values, " ++) . explain p . map snd . toList
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
approxEq :: (RealFloat a, Show a) => a -> Predicate a
approxEq x = withDefaultExplain show " " $ \explainImpl ->
  Predicate
    { showPredicate = "≈ " ++ show x,
      showNegation = "≇" ++ show x,
      accept = \y -> abs (x - y) < diff,
      explain = explainImpl
    }
  where
    diff = encodeFloat 1 (snd (decodeFloat x) + floatDigits x `div` 2)

-- | A 'Predicate' that accepts positive numbers of any 'Ord'ered 'Num' type.
--
-- >>> accept positive 1
-- True
--
-- >>> accept positive 0
-- False
--
-- >>> accept positive (-1)
-- False
positive :: (Ord a, Num a) => Predicate a
positive =
  Predicate
    { showPredicate = "positive",
      showNegation = "non-positive",
      accept = \x -> signum x > 0,
      explain = \x ->
        if
            | signum x > 0 -> "value is positive"
            | x == 0 -> "value is zero"
            | signum x < 0 -> "value is negative"
            | otherwise -> "value has unknown sign"
    }

-- | A 'Predicate' that accepts negative numbers of any 'Ord'ered 'Num' type.
--
-- >>> accept negative 1
-- False
--
-- >>> accept negative 0
-- False
--
-- >>> accept negative (-1)
-- True
negative :: (Ord a, Num a) => Predicate a
negative =
  Predicate
    { showPredicate = "negative",
      showNegation = "non-negative",
      accept = \x -> signum x < 0,
      explain = \x ->
        if
            | signum x < 0 -> "value is negative"
            | x == 0 -> "value is zero"
            | signum x < 0 -> "value is positive"
            | otherwise -> "value has unknown sign"
    }

-- | A 'Predicate' that accepts non-positive numbers of any 'Ord'ered 'Num'
-- type.
--
-- >>> accept nonPositive 1
-- False
--
-- >>> accept nonPositive 0
-- True
--
-- >>> accept nonPositive (-1)
-- True
nonPositive :: (Ord a, Num a) => Predicate a
nonPositive = notP positive

-- | A 'Predicate' that accepts non-negative numbers of any 'Ord'ered 'Num'
-- type.
--
-- >>> accept nonNegative 1
-- True
--
-- >>> accept nonNegative 0
-- True
--
-- >>> accept nonNegative (-1)
-- False
nonNegative :: (Ord a, Num a) => Predicate a
nonNegative = notP negative

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
      showNegation = "non-finite",
      accept = isFinite,
      explain = \x ->
        if isFinite x
          then "value is finite"
          else "value is not finite"
    }
  where
    isFinite x = not (isInfinite x) && not (isNaN x)

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
      showNegation = "non-infinite",
      accept = isInfinite,
      explain = \x ->
        if isInfinite x
          then "value is infinite"
          else "value is not infinite"
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
      showNegation = "non-NaN",
      accept = isNaN,
      explain = \x ->
        if isNaN x
          then "value is NaN"
          else "value is not NaN"
    }

-- | A conversion from @a -> 'Bool'@ to 'Predicate'.  This is a fallback that
-- can be used to build a 'Predicate' that checks anything at all.  However, its
-- description will be less helpful than standard 'Predicate's.
--
-- >>> accept (is even) 3
-- False
-- >>> accept (is even) 4
-- True
is :: HasCallStack => (a -> Bool) -> Predicate a
is p =
  Predicate
    { showPredicate = withLoc (locate callStack "custom predicate"),
      showNegation = withLoc (locate callStack "negated custom predicate"),
      accept = p,
      explain = \x ->
        if p x
          then "value matched custom predicate"
          else "value did not match custom predicate"
    }

-- | A Template Haskell splice that acts like 'is', but receives a quoted
-- expression at compile time and has a more helpful description for error
-- messages.
--
-- >>> accept $(qIs [| even |]) 3
-- False
-- >>> accept $(qIs [| even |]) 4
-- True
--
-- >>> show $(qIs [| even |])
-- "even"
qIs :: HasCallStack => ExpQ -> ExpQ
qIs p =
  [|
    Predicate
      { showPredicate = $description,
        showNegation = "not " ++ $description,
        accept = $p,
        explain = \x -> if $p x then $description else "not " ++ $description
      }
    |]
  where
    description = lift . pprint . removeModNames =<< p

-- | A combinator to lift a 'Predicate' to work on a property or computed value
-- of the original value.
--
-- >>> accept (with abs (gt 5)) (-6)
-- True
-- >>> accept (with abs (gt 5)) (-5)
-- False
-- >>> accept (with reverse (eq "olleh")) "hello"
-- True
-- >>> accept (with reverse (eq "olleh")) "goodbye"
-- False
with :: HasCallStack => (a -> b) -> Predicate b -> Predicate a
with f p =
  Predicate
    { showPredicate = prop ++ ": " ++ show p,
      showNegation = prop ++ ": " ++ showNegation p,
      accept = accept p . f,
      explain = ((prop ++ ": ") ++) . explain p . f
    }
  where
    prop = withLoc (locate callStack "property")

-- | A Template Haskell splice that acts like 'is', but receives a quoted typed
-- expression at compile time and has a more helpful description for error
-- messages.
--
-- >>> accept ($(qWith [| abs |]) (gt 5)) (-6)
-- True
-- >>> accept ($(qWith [| abs |]) (gt 5)) (-5)
-- False
-- >>> accept ($(qWith [| reverse |]) (eq "olleh")) "hello"
-- True
-- >>> accept ($(qWith [| reverse |]) (eq "olleh")) "goodbye"
-- False
--
-- >>> show ($(qWith [| abs |]) (gt 5))
-- "abs: > 5"
qWith :: ExpQ -> ExpQ
qWith f =
  [|
    \p ->
      Predicate
        { showPredicate = $prop ++ ": " ++ show p,
          showNegation = $prop ++ ": " ++ showNegation p,
          accept = accept p . $f,
          explain = (($prop ++ ": ") ++) . explain p . $f
        }
    |]
  where
    prop = lift . pprint . removeModNames =<< f

-- | A Template Haskell splice that turns a quoted pattern into a predicate that
-- accepts values that match the pattern.
--
-- >>> accept $(qMatch [p| Just (Left _) |]) Nothing
-- False
-- >>> accept $(qMatch [p| Just (Left _) |]) (Just (Left 5))
-- True
-- >>> accept $(qMatch [p| Just (Left _) |]) (Just (Right 5))
-- False
--
-- >>> show $(qMatch [p| Just (Left _) |])
-- "Just (Left _)"
qMatch :: PatQ -> ExpQ
qMatch qpat =
  [|
    Predicate
      { showPredicate = $patString,
        showNegation = "not " ++ $patString,
        accept = \case
          $qpat -> True
          _ -> False,
        explain = \case
          $qpat -> "value matched " ++ $patString
          _ -> "value didn't match " ++ $patString
      }
    |]
  where
    patString = lift . pprint . removeModNames =<< qpat

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
      showNegation =
        "not " ++ showPredicate p ++ " :: "
          ++ show (typeRep (Proxy :: Proxy a)),
      accept = \x -> case cast x of
        Nothing -> False
        Just y -> accept p y,
      explain = \x -> case cast x of
        Nothing ->
          "wrong type ("
            ++ show (typeRep (undefined :: Proxy b))
            ++ " vs. "
            ++ show (typeRep (undefined :: Proxy a))
            ++ ")"
        Just y -> explain p y
    }
