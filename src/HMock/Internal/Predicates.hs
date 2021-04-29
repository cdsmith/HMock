module HMock.Internal.Predicates where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import GHC.Stack
  ( HasCallStack,
    callStack,
    getCallStack,
    withFrozenCallStack,
  )

data Predicate a = Predicate
  { showPredicate :: String,
    accept :: a -> Bool
  }

eq_ :: (Show a, Eq a) => a -> Predicate a
eq_ x =
  Predicate
    { showPredicate = show x,
      accept = (== x)
    }

neq_ :: (Show a, Eq a) => a -> Predicate a
neq_ x =
  Predicate
    { showPredicate = "≠ " ++ show x,
      accept = (/= x)
    }

gt_ :: (Show a, Ord a) => a -> Predicate a
gt_ x =
  Predicate
    { showPredicate = "> " ++ show x,
      accept = (> x)
    }

geq_ :: (Show a, Ord a) => a -> Predicate a
geq_ x =
  Predicate
    { showPredicate = ">= " ++ show x,
      accept = (>= x)
    }

lt_ :: (Show a, Ord a) => a -> Predicate a
lt_ x =
  Predicate
    { showPredicate = "< " ++ show x,
      accept = (< x)
    }

leq_ :: (Show a, Ord a) => a -> Predicate a
leq_ x =
  Predicate
    { showPredicate = "<= " ++ show x,
      accept = (<= x)
    }

any_ :: Predicate a
any_ =
  Predicate
    { showPredicate = "any",
      accept = const True
    }

none_ :: Predicate a
none_ =
  Predicate
    { showPredicate = "none",
      accept = const False
    }

and_ :: Predicate a -> Predicate a -> Predicate a
p `and_` q =
  Predicate
    { showPredicate = showPredicate p ++ " and " ++ showPredicate q,
      accept = \x -> accept p x && accept q x
    }

or_ :: Predicate a -> Predicate a -> Predicate a
p `or_` q =
  Predicate
    { showPredicate = showPredicate p ++ " or " ++ showPredicate q,
      accept = \x -> accept p x || accept q x
    }

not_ :: Predicate a -> Predicate a
not_ p =
  Predicate
    { showPredicate = "not " ++ showPredicate p,
      accept = not . accept p
    }

startsWith_ :: String -> Predicate String
startsWith_ s =
  Predicate
    { showPredicate = "starts with " ++ show s,
      accept = \x -> s `isPrefixOf` x
    }

endsWith_ :: String -> Predicate String
endsWith_ s =
  Predicate
    { showPredicate = "ends with " ++ show s,
      accept = \x -> s `isSuffixOf` x
    }

hasSubstr_ :: String -> Predicate String
hasSubstr_ s =
  Predicate
    { showPredicate = "has substring " ++ show s,
      accept = \x -> s `isInfixOf` x
    }

suchThat_ :: HasCallStack => (a -> Bool) -> Predicate a
suchThat_ f =
  withFrozenCallStack $
    Predicate
      { showPredicate = case locs of
          (loc : _) -> "custom predicate at " ++ show loc
          _ -> "custom predicate",
        accept = f
      }
  where
    locs = map snd (getCallStack callStack)
