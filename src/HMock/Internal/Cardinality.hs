module HMock.Internal.Cardinality where

-- | An acceptable range of times for an action to be performed.
--
-- A cardinality can have a lower and an upper bound.
data Cardinality = Interval Int (Maybe Int) deriving (Eq)

instance Show Cardinality where
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

-- | Decrements a 'Cardinality'.
--
-- Returns 'Just' the remaining 'Cardinality', if any, or 'Nothing' if there is
-- none left.
decCardinality :: Cardinality -> Maybe Cardinality
decCardinality (Interval _ (Just hi))
  | hi <= 1 = Nothing
decCardinality (Interval lo hi) =
  Just (Interval (max 0 (lo - 1)) (subtract 1 <$> hi))

-- | A 'Cardinality' that means exactly this many times.
exactly :: Int -> Cardinality
exactly n = Interval n (Just n)

-- | A 'Cardinality' that means exactly once.
once :: Cardinality
once = exactly 1

-- | A 'Cardinality' that means any number of times.
anyCardinality :: Cardinality
anyCardinality = atLeast 0

-- | A 'Cardinality' that means at least this many times.
atLeast :: Int -> Cardinality
atLeast n = Interval n Nothing

-- | A 'Cardinality' that means at most this many times.
atMost :: Int -> Cardinality
atMost n = Interval 0 (Just n)

-- | A 'Cardinality' that means any number in this interval, endpoints included.
-- For example, @'interval' 2 3@ means 2 or 3 times, while @'interval' n n@ is
-- equivalent to @'exactly' n@.
interval :: Int -> Int -> Cardinality
interval m n = Interval (min m n) (Just (max m n))
