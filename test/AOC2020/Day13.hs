{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day13 where

import AOC2020.Common
import Data.Foldable
import Data.List.Split
import Data.Ord
import Lib
import Test.Hspec

parseTT :: String -> (Int, [(Int, Int)])
parseTT str = (read arr, sched')
 where
  [arr, sched] = lines str
  sched' =
    zip (splitOn "," sched) [0 ..] >>= \case
      ("x", _) -> []
      (n, i) -> [(read n, i)]

-- https://adventofcode.com/2020/day/13

day13 :: Spec
day13 = do
  (arr, bs) <- runIO $ parseTT <$> readFile "input/day13.txt"
  star1 2045 . uncurry (*) . minimumBy (comparing snd) . fmap (\(b, _) -> (b, b - mod arr b)) $ bs
  star2 402251700208309 . crt . fmap (\(b, off) -> (b - off, b)) $ bs
