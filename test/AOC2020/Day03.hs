{-# LANGUAGE OverloadedStrings #-}

module AOC2020.Day03 (day3) where

import Block
import Control.Lens
import Linear
import Parse
import TestLib

day3 :: Spec
day3 = do
  block <- runIO $ parseFile "input/2020/day3.txt" pBlock
  star1 195 $ tobbogan (V2 3 1) block
  star2 3772314000 $
    product
      [ tobbogan (V2 1 1) block,
        tobbogan (V2 3 1) block,
        tobbogan (V2 5 1) block,
        tobbogan (V2 7 1) block,
        tobbogan (V2 1 2) block
      ]

tobbogan :: V2 Int -> Block Char -> Int
tobbogan d b = length $ filter (== '#') $ go 0
  where
    go :: V2 Int -> [Char]
    go v
      | v ^. _y < bSize b ^. _y = bIndexWrap v b : go (v + d)
      | otherwise = []
