{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day01 (day1) where

import Lib
import Linear
import Parse
import Test.Hspec
import TestLib

day1 :: Spec
day1 = do
  input :: [Int] <- runIO $ parseFile "input/2020/day1.txt" $ pLines decimal
  star1 876459
    . product
    . fmap product
    . filter ((== 2020) . sum)
    . uniqueTuples @V2
    $ input
  star2 116168640
    . product
    . fmap product
    . filter ((== 2020) . sum)
    . uniqueTuples @V3
    $ input
