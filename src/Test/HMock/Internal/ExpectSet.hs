{-# LANGUAGE GADTSyntax #-}

module Test.HMock.Internal.ExpectSet where

import Control.Arrow (second)
import Test.HMock.Internal.Multiplicity
  ( Multiplicity,
    decMultiplicity,
    exhaustable,
    once,
  )

data Order = InOrder | AnyOrder deriving (Eq)

class Steppable step where
  nextStep :: step -> step

-- | A set of expected actions and their responses.  An entire test with mocks
-- is expected to run in a single base 'Monad', which is the first type
-- parameter here.  The second parameter is just a trick with `ExpectContext`
-- (see below) to avoid GHC warnings about unused return values.
data ExpectSet step where
  ExpectNothing :: ExpectSet step
  Expect :: Multiplicity -> step -> ExpectSet step
  ExpectMulti :: Order -> [ExpectSet step] -> ExpectSet step

-- | Converts a set of expectations into a string that summarizes them, with
-- the given prefix (used to indent).
formatExpectSet :: Show step => String -> ExpectSet step -> String
formatExpectSet prefix ExpectNothing = prefix ++ "nothing"
formatExpectSet prefix (Expect multiplicity step) =
  prefix ++ show step ++ mult
  where
    mult
      | multiplicity == once = ""
      | otherwise = " [" ++ show multiplicity ++ "]"
formatExpectSet prefix (ExpectMulti order xs) =
  let label = if order == InOrder then "in sequence:\n" else "in any order:\n"
   in prefix ++ label ++ unlines (map (formatExpectSet (prefix ++ "  ")) xs)

-- | Get a list of steps that can match actions right now, together with the
-- remaining expectations if each one were to match.
liveSteps :: Steppable step => ExpectSet step -> [(step, ExpectSet step)]
liveSteps = map (second simplify) . go
  where
    go ExpectNothing = []
    go (Expect multiplicity step) = case decMultiplicity multiplicity of
      Nothing -> [(step, ExpectNothing)]
      Just multiplicity' -> [(step, Expect multiplicity' (nextStep step))]
    go (ExpectMulti order es) = fmap (ExpectMulti order) <$> nextSteps order es

    nextSteps _ [] = []
    nextSteps order (e : es)
      | AnyOrder <- order = eOptions ++ map (fmap (e :)) esOptions
      | ExpectNothing <- excess e = eOptions ++ esOptions
      | otherwise = eOptions
      where
        eOptions = fmap (: es) <$> go e
        esOptions = nextSteps order es

-- | Simplifies a set of expectations.  This removes unnecessary occurrences of
-- 'ExpectNothing' and collapses nested lists with the same ordering
-- constraints.
simplify :: ExpectSet step -> ExpectSet step
simplify e = case e of
  (ExpectMulti order xs) -> simplifyMulti order xs
  _ -> e
  where
    simplifyMulti order =
      construct order . concatMap (expand order . simplify)

    expand :: Order -> ExpectSet step -> [ExpectSet step]
    expand _ ExpectNothing = []
    expand order (ExpectMulti order' xs) | order == order' = xs
    expand _ other = [other]

    construct _ [] = ExpectNothing
    construct _ [x] = x
    construct order xs = ExpectMulti order xs

-- | Reduces a set of expectations to the minimum steps that would be required
-- to satisfy the entire set.  This weeds out unnecessary information before
-- reporting that there were unmet expectations at the end of the test.
excess :: ExpectSet step -> ExpectSet step
excess = simplify . go
  where
    go :: ExpectSet step -> ExpectSet step
    go ExpectNothing = ExpectNothing
    go e@(Expect mult _)
      | exhaustable mult = ExpectNothing
      | otherwise = e
    go (ExpectMulti order xs) = ExpectMulti order (map go xs)
