{-# LANGUAGE DeriveDataTypeable #-}

module Test.HMock.Internal.Multiplicity where

-- | An acceptable range of number of times for something to happen.
--
-- A multiplicity can have a lower and an upper bound.
data Multiplicity = Multiplicity Int (Maybe Int) deriving (Eq)

instance Show Multiplicity where
  show m | not (feasible m) = "infeasible"
  show (Multiplicity 0 (Just 0)) = "never"
  show (Multiplicity 1 (Just 1)) = "once"
  show (Multiplicity 2 (Just 2)) = "twice"
  show (Multiplicity 0 Nothing) = "any number of times"
  show (Multiplicity 1 Nothing) = "at least once"
  show (Multiplicity 2 Nothing) = "at least twice"
  show (Multiplicity n Nothing) = "at least " ++ show n ++ " times"
  show (Multiplicity 0 (Just 1)) = "at most once"
  show (Multiplicity 0 (Just 2)) = "at most twice"
  show (Multiplicity 0 (Just n)) = "at most " ++ show n ++ " times"
  show (Multiplicity m (Just n))
    | m == n = show n ++ " times"
    | m == n - 1 = show m ++ " or " ++ show n ++ " times"
    | otherwise = show m ++ " to " ++ show n ++ " times"

-- | A 'Multiplicity' value representing inconsistent expectations.
infeasible :: Multiplicity
infeasible = Multiplicity 0 (Just (-1))

-- | This is an incomplete instance, provided for convenience.
--
-- >>> meetsMultiplicity 5 4
-- False
-- >>> meetsMultiplicity 5 5
-- True
-- >>> between 4 6 - between 1 2
-- 2 to 5 times
instance Num Multiplicity where
  fromInteger n
    | n < 0 = infeasible
    | otherwise =
      normalize $
        Multiplicity (fromInteger n) (Just (fromInteger n))

  m1@(Multiplicity a b) + m2@(Multiplicity c d)
    | feasible m1 && feasible m2 =
      normalize $ Multiplicity (a + c) ((+) <$> b <*> d)
    | otherwise = infeasible

  m1@(Multiplicity a b) - m2@(Multiplicity c d)
    | feasible m1 && feasible m2 =
      normalize $ Multiplicity (maybe 0 (a -) d) (subtract c <$> b)
    | otherwise = infeasible

  (*) = error "Multiplicities are not closed under multiplication"

  abs = id

  signum (Multiplicity 0 (Just 0)) = 0
  signum _ = 1

normalize :: Multiplicity -> Multiplicity
normalize (Multiplicity a b) = Multiplicity (max a 0) b

-- | Checks whether a certain number satisfies the 'Multiplicity'.
meetsMultiplicity :: Multiplicity -> Int -> Bool
meetsMultiplicity (Multiplicity lo mbhi) n
  | n < lo = False
  | Just hi <- mbhi, n > hi = False
  | otherwise = True

-- | A 'Multiplicity' that means exactly once.
--
-- >>> meetsMultiplicity once 0
-- False
-- >>> meetsMultiplicity once 1
-- True
-- >>> meetsMultiplicity once 2
-- False
once :: Multiplicity
once = 1

-- | A 'Multiplicity' that means any number of times.
-- >>> meetsMultiplicity anyMultiplicity 0
-- True
-- >>> meetsMultiplicity anyMultiplicity 1
-- True
-- >>> meetsMultiplicity anyMultiplicity 10
-- True
anyMultiplicity :: Multiplicity
anyMultiplicity = atLeast 0

-- | A 'Multiplicity' that means at least this many times.
--
-- >>> meetsMultiplicity (atLeast 2) 1
-- False
-- >>> meetsMultiplicity (atLeast 2) 2
-- True
-- >>> meetsMultiplicity (atLeast 2) 3
-- True
atLeast :: Multiplicity -> Multiplicity
atLeast (Multiplicity n _) = Multiplicity n Nothing

-- | A 'Multiplicity' that means at most this many times.
--
-- >>> meetsMultiplicity (atMost 2) 1
-- True
-- >>> meetsMultiplicity (atMost 2) 2
-- True
-- >>> meetsMultiplicity (atMost 2) 3
-- False
atMost :: Multiplicity -> Multiplicity
atMost (Multiplicity _ n) = Multiplicity 0 n

-- | A 'Multiplicity' that means any number in this interval, endpoints
-- included.  For example, @'between' 2 3@ means 2 or 3 times, while
-- @'between' n n@ is equivalent to @n@.
--
-- >>> meetsMultiplicity (between 2 3) 1
-- False
-- >>> meetsMultiplicity (between 2 3) 2
-- True
-- >>> meetsMultiplicity (between 2 3) 3
-- True
-- >>> meetsMultiplicity (between 2 3) 4
-- False
between :: Multiplicity -> Multiplicity -> Multiplicity
between (Multiplicity m _) (Multiplicity _ n) = Multiplicity m n

-- | Checks whether a 'Multiplicity' includes zero in its range.
--
-- >>> exhaustable anyMultiplicity
-- True
-- >>> exhaustable (atLeast 2)
-- False
-- >>> exhaustable (atMost 3)
-- True
-- >>> exhaustable (between 0 2)
-- True
exhaustable :: Multiplicity -> Bool
exhaustable m@(Multiplicity lo _) = feasible m && lo == 0

-- | Checks whether a 'Multiplicity' is capable of matching any number at all.
--
-- >>> feasible once
-- True
-- >>> feasible 0
-- True
-- >>> feasible (once - 2)
-- False
feasible :: Multiplicity -> Bool
feasible (Multiplicity a b) = maybe True (>= a) b
