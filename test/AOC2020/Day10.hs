{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day10 (day10) where

import AOC2020.Common
import Data.List (foldl', sort)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Test.Hspec

day10 :: Spec
day10 = do
  nums :: [Int] <- runIO $ fmap read . lines <$> readFile "input/2020/day10.txt"
  let go :: Int -> Int -> Int -> [Int] -> Int
      go n n1 n3 [] = (n1 * (n3 + 1))
      go n n1 n3 (x : xs)
        | x - n == 1 = go x (n1 + 1) n3 xs
        | otherwise = go x n1 (n3 + 1) xs
  star1 2048 $ go 0 0 0 nums
  let goal = maximum nums + 3
  let f :: Int -> Map Int Int -> Map Int Int
      f n m =
        let ways = sum $ (\n' -> fromMaybe 0 (M.lookup n' m)) <$> ([(n -3) .. (n -1)])
         in M.insert n ways m
      m = foldl' (flip f) (M.singleton 0 1) (sort (goal : nums))
  star2 1322306994176 $ m M.! goal
