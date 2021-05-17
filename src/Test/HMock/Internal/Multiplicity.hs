module Test.HMock.Internal.Multiplicity where

-- | An acceptable range of number of times for something to happen.
--
-- A multiplicity can have a lower and an upper bound.
data Multiplicity = Multiplicity Int (Maybe Int) deriving (Eq)

instance Show Multiplicity where
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

-- | Checks whether a certain number satisfies the 'Multiplicity'.
meetsMultiplicity :: Multiplicity -> Int -> Bool
meetsMultiplicity (Multiplicity lo mbhi) n
  | n < lo = False
  | Just hi <- mbhi, n > hi = False
  | otherwise = True

-- | Decrements a 'Multiplicity'.
--
-- Returns 'Just' the remaining 'Multiplicity', if any, or 'Nothing' if there is
-- none left.
decMultiplicity :: Multiplicity -> Maybe Multiplicity
decMultiplicity (Multiplicity _ (Just hi))
  | hi <= 1 = Nothing
decMultiplicity (Multiplicity lo hi) =
  Just (Multiplicity (max 0 (lo - 1)) (subtract 1 <$> hi))

-- | A 'Multiplicity' that means exactly this many times.
--
-- >>> meetsMultiplicity (exactly 5) 4
-- False
-- >>> meetsMultiplicity (exactly 5) 5
-- True
-- >>> meetsMultiplicity (exactly 5) 6
-- False
exactly :: Int -> Multiplicity
exactly n = Multiplicity n (Just n)

-- | A 'Multiplicity' that means exactly once.
--
-- >>> meetsMultiplicity once 0
-- False
-- >>> meetsMultiplicity once 1
-- True
-- >>> meetsMultiplicity once 2
-- False
once :: Multiplicity
once = exactly 1

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
atLeast :: Int -> Multiplicity
atLeast n = Multiplicity n Nothing

-- | A 'Multiplicity' that means at most this many times.
--
-- >>> meetsMultiplicity (atMost 2) 1
-- True
-- >>> meetsMultiplicity (atMost 2) 2
-- True
-- >>> meetsMultiplicity (atMost 2) 3
-- False
atMost :: Int -> Multiplicity
atMost n = Multiplicity 0 (Just n)

-- | A 'Multiplicity' that means any number in this interval, endpoints
-- included.  For example, @'interval' 2 3@ means 2 or 3 times, while
-- @'interval' n n@ is equivalent to @'exactly' n@.
--
-- >>> meetsMultiplicity (interval 2 3) 1
-- False
-- >>> meetsMultiplicity (interval 2 3) 2
-- True
-- >>> meetsMultiplicity (interval 2 3) 3
-- True
-- >>> meetsMultiplicity (interval 2 3) 4
-- False
interval :: Int -> Int -> Multiplicity
interval m n = Multiplicity (min m n) (Just (max m n))

-- | Checks whether a 'Multiplicity' includes zero in its range.
--
-- >>> exhaustable anyMultiplicity
-- True
-- >>> exhaustable (atLeast 2)
-- False
-- >>> exhaustable (atMost 3)
-- True
-- >>> exhaustable (interval 0 2)
-- True
exhaustable :: Multiplicity -> Bool
exhaustable (Multiplicity lo _) = lo == 0
