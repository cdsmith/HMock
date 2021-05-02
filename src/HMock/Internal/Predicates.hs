{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HMock.Internal.Predicates where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Typeable
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

eq :: (Show a, Eq a) => a -> Predicate a
eq x =
  Predicate
    { showPredicate = show x,
      accept = (== x)
    }

neq :: (Show a, Eq a) => a -> Predicate a
neq x =
  Predicate
    { showPredicate = "≠ " ++ show x,
      accept = (/= x)
    }

gt :: (Show a, Ord a) => a -> Predicate a
gt x =
  Predicate
    { showPredicate = "> " ++ show x,
      accept = (> x)
    }

geq :: (Show a, Ord a) => a -> Predicate a
geq x =
  Predicate
    { showPredicate = ">= " ++ show x,
      accept = (>= x)
    }

lt :: (Show a, Ord a) => a -> Predicate a
lt x =
  Predicate
    { showPredicate = "< " ++ show x,
      accept = (< x)
    }

leq :: (Show a, Ord a) => a -> Predicate a
leq x =
  Predicate
    { showPredicate = "<= " ++ show x,
      accept = (<= x)
    }

__ :: Predicate a
__ =
  Predicate
    { showPredicate = "any",
      accept = const True
    }

andP :: Predicate a -> Predicate a -> Predicate a
p `andP` q =
  Predicate
    { showPredicate = showPredicate p ++ " and " ++ showPredicate q,
      accept = \x -> accept p x && accept q x
    }

orP :: Predicate a -> Predicate a -> Predicate a
p `orP` q =
  Predicate
    { showPredicate = showPredicate p ++ " or " ++ showPredicate q,
      accept = \x -> accept p x || accept q x
    }

notP :: Predicate a -> Predicate a
notP p =
  Predicate
    { showPredicate = "not " ++ showPredicate p,
      accept = not . accept p
    }

startsWith :: String -> Predicate String
startsWith s =
  Predicate
    { showPredicate = "starts with " ++ show s,
      accept = \x -> s `isPrefixOf` x
    }

endsWith :: String -> Predicate String
endsWith s =
  Predicate
    { showPredicate = "ends with " ++ show s,
      accept = \x -> s `isSuffixOf` x
    }

hasSubstr :: String -> Predicate String
hasSubstr s =
  Predicate
    { showPredicate = "has substring " ++ show s,
      accept = \x -> s `isInfixOf` x
    }

suchThat :: HasCallStack => (a -> Bool) -> Predicate a
suchThat f =
  withFrozenCallStack $
    Predicate
      { showPredicate = case locs of
          (loc : _) -> "custom predicate at " ++ show loc
          _ -> "custom predicate",
        accept = f
      }
  where
    locs = map snd (getCallStack callStack)

-- | Converts a 'Predicate' to a new type.  Typically used with visible type
-- application, as in @'typed' @Int ('lt' 42)@.  This will only match if the
-- argument is an Int, and also less than 42.
typed :: forall a b. (Typeable a, Typeable b) => Predicate a -> Predicate b
typed p =
  Predicate
    { showPredicate =
        showPredicate p ++ " :: " ++ show (typeRep (Proxy :: Proxy a)),
      accept = \x -> case cast x of
        Nothing -> False
        Just y -> accept p y
    }
