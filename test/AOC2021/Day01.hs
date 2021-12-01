{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-imports #-}

module AOC2021.Day01 where

import Lib
import TestLib

ex1 :: String -> Int
ex1 = length . filter (ordered (<)) . slide2 . fmap (read @Int) . lines

ex2 :: String -> Int
ex2 = length . filter (ordered (<)) . slide2 . fmap sum . slide3 . fmap (read @Int) . lines

day1 :: Spec
day1 = do
  ex <- runIO $ readFile "input/2021/01.ex.txt"
  input <- runIO $ readFile "input/2021/01.txt"
  star1ex 7 $ ex1 ex
  star1 1759 $ ex1 input
  star2ex 5 $ ex2 ex
  star2 1805 $ ex2 input

-- putStrLn d1
