{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day09 (day9) where

import AOC2020.Common
import Data.Foldable (toList)
import Data.Sequence qualified as Q
import Lib
import Linear hiding (E)

f :: Int -> [Int] -> [(Q.Seq Int, Int)]
f n l = go (Q.fromList $ take n l) (drop n l)
 where
  go _ [] = []
  go q (n : ns) = (q, n) : go (Q.drop 1 q Q.:|> n) ns

valid :: (Q.Seq Int, Int) -> Bool
valid (p, q) = any ((== q) . sum) $ uniqueTuples @V2 (toList p)

look :: Int -> [Int] -> [Int]
look goal = go
 where
  go (a : b : t) = maybe (go (b : t)) id $ single (a + b) [b, a] t
  go _ = []
  single :: Int -> [Int] -> [Int] -> Maybe [Int]
  single _ _ [] = Nothing
  single n run (h : t) = case compare n goal of
    EQ -> Just run
    LT -> single (n + h) (h : run) t
    GT -> Nothing

day9 :: Spec
day9 = do
  nums :: [Int] <- runIO $ fmap read . lines <$> readFile "input/day9.txt"
  star1 375054920 $ snd . head . filter (not . valid) $ f 25 nums
  let [(_, inv)] = filter (not . valid) $ f 25 nums
  let run = look inv nums
  star2 54142584 $ maximum run + minimum run
