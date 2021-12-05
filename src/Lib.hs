{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Applicative hiding (many)
import Control.Monad.Reader
import Control.Monad.State
import Data.Bool
import Data.Foldable (toList)
import Data.Maybe
import Linear

-- bigrams [a, b, c] = [(a,b), (b,c)]
bigrams :: [a] -> [(a, a)]
bigrams xs = zip xs (tail xs)

-- ordered (<=) = ascending
-- ordered (<) = strictly ascending
ordered :: Foldable f => (a -> a -> Bool) -> f a -> Bool
ordered f = all (uncurry f) . bigrams . toList

slide2 :: [a] -> [V2 a]
slide2 xs = zipWith V2 xs (tail xs)

slide3 :: [a] -> [V3 a]
slide3 xs = (\(a, b, c) -> V3 a b c) <$> zip3 xs (drop 1 xs) (drop 2 xs)

count :: Foldable t => (a -> Bool) -> t a -> Int
count p = length . filter p . toList

adjacent :: (Eq (t Int), Traversable t) => t Int -> [t Int]
adjacent v = filter (/= v) . traverse (\x -> [x - 1 .. x + 1]) $ v

orthogonal :: (Eq (t Int), Traversable t, Applicative t) => t Int -> [t Int]
orthogonal v = filter (\v' -> sum (liftA2 (\a b -> abs (a - b)) v' v) == 1) $ adjacent v

range :: Int -> Int -> [Int]
range a b
  | a < b = [a .. b]
  | otherwise = reverse [b .. a]

-- Not very clever; just repeatedly filters out candidates
-- that already uniquely belong to another field
assignUnique :: Eq c => [(r, [c])] -> Maybe [(r, c)]
assignUnique rs
  | length singles == length rs = Just singles
  | null singles = Nothing
  | otherwise = assignUnique rs'
  where
    singles =
      rs >>= \case
        (r, [c]) -> [(r, c)]
        _ -> []
    solveds = snd <$> singles
    rs' = flip fmap rs $ \case
      (r, [c]) -> (r, [c])
      (r, cs) -> (r, filter (`notElem` solveds) cs)

findFix :: Eq a => (a -> a) -> a -> a
findFix f = go where go a = let a' = f a in if a == a' then a else go a'

fromBE :: [Bool] -> Int
fromBE = go 0
  where
    go n [] = n
    go n (b : t) = go (2 * n + bool 0 1 b) t

-- https://en.wikipedia.org/wiki/Chinese_remainder_theorem
-- https://rosettacode.org/wiki/Chinese_remainder_theorem
-- https://www.youtube.com/watch?v=zIFehsBHB8o
-- argument is a list of pairs (b,z)
-- result is x such that for all (b,z), x%z = b
crt :: [(Int, Int)] -> Int
crt pairs = sum (f <$> pairs) `mod` n
  where
    n = product $ snd <$> pairs

    f (b', z') =
      let n' = div n z'
          x' = fromMaybe (error $ "no modular inverse for " <> show (n', b')) $ modInv n' z'
       in b' * n' * x'

-- modular inverse x of b wrt. z
-- i.e. (x * b) % z = 1
modInv :: Int -> Int -> Maybe Int
modInv b z =
  let (x, y) = eea b z
   in if b * x + z * y == 1
        then Just x
        else Nothing

-- https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
-- given two numbers, finds x and y such that x*a + y*b = gcd(a,b)
-- i.e. eea 2 3 = (-1,1)
eea :: Int -> Int -> (Int, Int)
eea _ 0 = (1, 0)
eea a b =
  let (s, t) = eea b r
      (q, r) = a `divMod` b
   in (t, s - q * t)

uniqueTuples :: (Traversable t, Applicative t) => [a] -> [t a]
uniqueTuples = uniques (pure ())
  where
    uniques :: Traversable t => t () -> [a] -> [t a]
    uniques base as =
      flip evalStateT as $
        forM base $
          const $
            fix $ \f ->
              get >>= \case
                [] -> empty
                (h : t) -> put t >> (pure h <|> f)
