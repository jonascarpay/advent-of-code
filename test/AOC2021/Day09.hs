{-# LANGUAGE OverloadedStrings #-}

module AOC2021.Day09 (day9) where

import Data.Char
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as M
import Lib
import Linear
import Parse
import Test.Hspec
import TestLib

type Input = Map (V2 Int) Int

parser :: Parser Input
parser = fmap toMap $ pLines $ many (toInt <$> alphaNumChar)
  where
    toInt :: Char -> Int
    toInt = subtract (ord '0') . ord

toMap :: [[Int]] -> Map (V2 Int) Int
toMap = M.fromList . coords

ex1 :: Input -> Int
ex1 input = sum $ (+ 1) . (input M.!) <$> lows
  where
    lows = filter isLow $ M.keys input
    isLow v = all (maybe True (> d) . (input M.!?)) (orthogonal v)
      where
        d = input M.! v

ex2 :: Input -> Int
ex2 input = product . take 3 . reverse . sort $ go ridges (M.keys ridges)
  where
    removeBasin :: [V2 Int] -> Int -> Map (V2 Int) Bool -> (Int, Map (V2 Int) Bool)
    removeBasin [] n m = (n, m)
    removeBasin (v : vs) n m =
      case M.lookup v m of
        Just False -> removeBasin (orthogonal v <> vs) (n + 1) (M.delete v m)
        _ -> removeBasin vs n m
    ridges = (== 9) <$> input
    go _ [] = []
    go m (root : t) =
      let (n, m') = removeBasin [root] 0 m
       in n : go m' t

day9 :: Spec
day9 = do
  exam <- runIO $ parseFile "input/2021/09.ex.txt" parser
  input <- runIO $ parseFile "input/2021/09.txt" parser
  star1ex 15 $ ex1 exam
  star1 631 $ ex1 input
  star2ex 1134 $ ex2 exam
  star2 821560 $ ex2 input
