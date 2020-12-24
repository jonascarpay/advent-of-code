module Histogram
  ( Histogram,
    toMap,
    increment,
    decrement,
    Histogram.lookup,
    (!),
    add,
    set,
    reset,
    zero,
    nonzero,
    empty,
    mapKeys,
    count,
    singleton,
    isSubsetOf,
    isSubsetOfBy,
    fromList,
    toList,
    allKeys,
  )
where

import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)

-- | A simple 'Data.Map'-based histogram for some key.
newtype Histogram k = Histogram
  { -- | Convert to a histogram to a map of all its nonzero values
    toMap :: M.Map k Int
  }
  deriving (Eq, Show)

instance Ord k => Semigroup (Histogram k) where
  Histogram m1 <> Histogram m2 = Histogram $ M.unionWith (+) m1 m2

instance Ord k => Monoid (Histogram k) where
  mempty = Histogram mempty
  mappend = (<>)

clip :: Int -> Maybe Int
clip n
  | n > 0 = Just n
  | otherwise = Nothing

-- | Increase a key's count by one
increment :: Ord k => k -> Histogram k -> Histogram k
increment k (Histogram m) = Histogram $ M.insertWith (+) k 1 m

-- | Decrease a key's count by one
decrement :: Ord k => k -> Histogram k -> Histogram k
decrement k (Histogram m) = Histogram $ M.update (clip . subtract 1) k m

-- | Increase a key's count by an arbitrary number.
--   Can also be used to decrease by passing a negative value.
--   If the count falls below zero, it's set to 0.
add :: Ord k => Int -> k -> Histogram k -> Histogram k
add n k (Histogram m) = Histogram $ M.alter f k m
  where
    f nOld = clip $ fromMaybe 0 nOld + n

-- | Set a key's count to an exact value.
--   Nonpositive numbers clip to 0.
set :: Ord k => Int -> k -> Histogram k -> Histogram k
set n k (Histogram m) = Histogram $ (if n > 0 then flip M.insert n else M.delete) k m

-- | Set a key's count to 0.
reset :: Ord k => k -> Histogram k -> Histogram k
reset k (Histogram m) = Histogram $ M.delete k m

-- | Check whether a key has a count of at least 1.
nonzero :: Ord k => k -> Histogram k -> Bool
nonzero k (Histogram m) = M.member k m

-- | Check whether a key has a count of 0
zero :: Ord k => k -> Histogram k -> Bool
zero k = not . nonzero k

-- | Check whether a histogram is empty
empty :: Histogram k -> Bool
empty (Histogram h) = null h

-- | Applies a function to every key.
--   If two keys in the original map to the same value, their counts are combined.
mapKeys :: Ord k2 => (k1 -> k2) -> Histogram k1 -> Histogram k2
mapKeys f (Histogram m) = Histogram $ M.mapKeysWith (+) f m

-- | A histogram containing one key with a count of 1.
singleton :: k -> Histogram k
singleton k = Histogram $ M.singleton k 1

-- | @isSubsetOfBy f h1 h2@ returns 'True' if every key in @h1@ compares to 'True' to its corresponding key in @h2@ by @f@.
isSubsetOfBy :: Ord k => (Int -> Int -> Bool) -> Histogram k -> Histogram k -> Bool
isSubsetOfBy f (Histogram h1) (Histogram h2) = M.isSubmapOfBy f h1 h2

-- | @isSubsetOf h1 h2@ returns 'True' if no key in has a greater count in @h1@ than in @h2@.
isSubsetOf :: Ord k => Histogram k -> Histogram k -> Bool
isSubsetOf = isSubsetOfBy (<=)

-- | Construct a histogram from a list of keys.
fromList :: Ord k => [k] -> Histogram k
fromList = foldr increment mempty

allKeys :: (Int -> Bool) -> Histogram k -> Bool
allKeys p (Histogram m) = all p m

toList :: Histogram k -> [(k, Int)]
toList (Histogram m) = M.toList m

lookup :: Ord k => k -> Histogram k -> Int
lookup k (Histogram m) = fromMaybe 0 (m M.!? k)

count :: Ord a => [a] -> [(a, Int)]
count = toList . fromList

(!) :: Ord k => Histogram k -> k -> Int
(!) = flip Histogram.lookup
