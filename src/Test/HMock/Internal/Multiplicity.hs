module Test.HMock.Internal.Multiplicity where

-- | An acceptable range of number of times for something to happen.
--
-- A multiplicity can have a lower and an upper bound.
data Multiplicity = Interval Int (Maybe Int) deriving (Eq)

instance Show Multiplicity where
  show (Interval 1 (Just 1)) = "once"
  show (Interval 2 (Just 2)) = "twice"
  show (Interval 0 Nothing) = "any number of times"
  show (Interval 1 Nothing) = "at least once"
  show (Interval 2 Nothing) = "at least twice"
  show (Interval n Nothing) = "at least " ++ show n ++ " times"
  show (Interval 0 (Just 1)) = "at most once"
  show (Interval 0 (Just 2)) = "at most twice"
  show (Interval 0 (Just n)) = "at most " ++ show n ++ " times"
  show (Interval m (Just n))
    | m == n = show n ++ " times"
    | m == n - 1 = show m ++ " or " ++ show n ++ " times"
    | otherwise = show m ++ " to " ++ show n ++ " times"

-- | Decrements a 'Multiplicity'.
--
-- Returns 'Just' the remaining 'Multiplicity', if any, or 'Nothing' if there is
-- none left.
decMultiplicity :: Multiplicity -> Maybe Multiplicity
decMultiplicity (Interval _ (Just hi))
  | hi <= 1 = Nothing
decMultiplicity (Interval lo hi) =
  Just (Interval (max 0 (lo - 1)) (subtract 1 <$> hi))

-- | A 'Multiplicity' that means exactly this many times.
exactly :: Int -> Multiplicity
exactly n = Interval n (Just n)

-- | A 'Multiplicity' that means exactly once.
once :: Multiplicity
once = exactly 1

-- | A 'Multiplicity' that means any number of times.
anyMultiplicity :: Multiplicity
anyMultiplicity = atLeast 0

-- | A 'Multiplicity' that means at least this many times.
atLeast :: Int -> Multiplicity
atLeast n = Interval n Nothing

-- | A 'Multiplicity' that means at most this many times.
atMost :: Int -> Multiplicity
atMost n = Interval 0 (Just n)

-- | A 'Multiplicity' that means any number in this interval, endpoints
-- included.  For example, @'interval' 2 3@ means 2 or 3 times, while
-- @'interval' n n@ is equivalent to @'exactly' n@.
interval :: Int -> Int -> Multiplicity
interval m n = Interval (min m n) (Just (max m n))
