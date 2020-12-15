{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module AOC2020.Day15 (day15) where

import AOC2020.Common
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as M
import Test.Hspec

input :: [Int]
input = [0, 13, 1, 16, 6, 17]

nums :: [Int]
nums = input <> go (length input - 1) seed (last input)
 where
  go :: Int -> IntMap Int -> Int -> [Int]
  go !turn lasts !last =
    let num = case M.lookup last lasts of
          Nothing -> 0
          Just n -> turn - n
     in num : go (turn + 1) (M.insert last turn lasts) num
  seed :: IntMap Int
  seed = M.fromList (zip (init input) [0 ..])

day15 :: Spec
day15 = do
  star1 234 (nums !! 2019)
  -- star2 8984 (nums !! 29999999)
  it "star 2" $ pendingWith "too slow to test"
