{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Applicative hiding (many)
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe

findFix :: Eq a => (a -> a) -> a -> a
findFix f = go where go a = let a' = f a in if a == a' then a else go a'

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
