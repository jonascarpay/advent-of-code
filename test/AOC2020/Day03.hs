{-# LANGUAGE OverloadedStrings #-}

module AOC2020.Day03 (day3) where

import AOC2020.Common
import Block
import Data.Word
import Parse
import Test.Hspec

day3 :: Spec
day3 = do
  block <- runIO $ parseFile "input/day3.txt" pBlock
  star1 195 $ tobbogan 3 1 block
  star2 3772314000 $
    product
      [ tobbogan 1 1 block
      , tobbogan 3 1 block
      , tobbogan 5 1 block
      , tobbogan 7 1 block
      , tobbogan 1 2 block
      ]

tobbogan :: Int -> Int -> Block Word8 -> Int
tobbogan dx dy b = length $ filter (== ascii '#') $ go 0 0
 where
  go :: Int -> Int -> [Word8]
  go x y
    | y < bHeight b = bIndexWrap x y b : go (x + dx) (y + dy)
    | otherwise = []
