{-# LANGUAGE OverloadedStrings #-}

module AOC2020.Day05 (day5) where

import Data.IntSet as IS
import Test.Hspec
import TestLib

day5 :: Spec
day5 = do
  ls <- runIO $ lines <$> readFile "input/2020/day5.txt"
  star1 861 $ maximum . fmap parseL $ ls
  star2 633 $ head . missing $ ls

missing :: [String] -> [Int]
missing str = [x | x <- [0 .. 1000], IS.member (x - 1) present && IS.member (x + 1) present && not (IS.member x present)]
  where
    present = IS.fromList $ parseL <$> str

parseL :: String -> Int
parseL = go 0
  where
    go n ('F' : t) = go (2 * n) t
    go n ('B' : t) = go (2 * n + 1) t
    go n ('L' : t) = go (2 * n) t
    go n ('R' : t) = go (2 * n + 1) t
    go n [] = n
