{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day10 (day10) where

import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as M
import Parse
import Test.Hspec
import TestLib

type Input = [Int]

parser :: Parser Input
parser = pLines decimal

ex1 :: Input -> Int
ex1 = go 0 0 0 . sort
  where
    go n1 n3 j (j' : js)
      | j' == j + 1 = go (n1 + 1) n3 j' js
      | j' == j + 2 = go n1 n3 j' js
      | j' == j + 3 = go n1 (n3 + 1) j' js
    go n1 n3 _ _ = n1 * (n3 + 1)

ex2 :: Input -> Int
ex2 = go (M.singleton 0 1) . sort
  where
    ways :: Int -> Map Int Int -> Int
    ways j m = let f i = M.findWithDefault 0 i m in f (j - 1) + f (j - 2) + f (j - 3)
    go :: Map Int Int -> [Int] -> Int
    go m [] = ways (maximum $ M.keys m) m
    go m (h : t) = go (M.insert h (ways h m) m) t

day10 :: Spec
day10 = do
  exam <- runIO $ parseFile "input/2020/day10ex.txt" parser
  input <- runIO $ parseFile "input/2020/day10.txt" parser
  star1ex 220 $ ex1 exam
  star1 2048 $ ex1 input
  star2ex 19208 $ ex2 exam
  star2 1322306994176 $ ex2 input
