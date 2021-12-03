{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-imports #-}

module AOC2021.Day02 where

import Lib
import TestLib

ex1 :: String -> Int
ex1 input =
  let go :: [[String]] -> Int -> Int -> Int
      go (["forward", n] : r) x y = go r (x + read n) y
      go (["up", n] : r) x y = go r x (y - read n)
      go (["down", n] : r) x y = go r x (y + read n)
      go [] x y = x * y
   in go (words <$> lines input) 0 0

ex2 :: String -> Int
ex2 input =
  let go :: [[String]] -> Int -> Int -> Int -> Int
      go (["forward", n] : r) x y a = go r (x + read n) (y + read n * a) a
      go (["up", n] : r) x y a = go r x y (a - read n)
      go (["down", n] : r) x y a = go r x y (a + read n)
      go [] x y a = x * y
   in go (words <$> lines input) 0 0 0

day2 :: Spec
day2 = do
  ex <- runIO $ readFile "input/2021/02.ex.txt"
  input <- runIO $ readFile "input/2021/02.txt"
  star1ex 150 $ ex1 ex
  star1 1604850 $ ex1 input
  star2ex 900 $ ex2 ex
  star2 1685186100 $ ex2 input

-- putStrLn d1
