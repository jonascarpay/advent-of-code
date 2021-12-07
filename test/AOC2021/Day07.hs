module AOC2021.Day07 where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Parse
import Test.Hspec
import TestLib

type Input = [Int]

parser :: Parser Input
parser = decimal `sepBy` char ','

ex1 :: Input -> Int
ex1 input = f target
  where
    target = minimumBy (comparing f) [minimum input .. maximum input]
    f n = sum $ fmap (abs . subtract n) input

cost :: Int -> Int
cost n = div (n * (n + 1)) 2

ex2 :: Input -> Int
ex2 input = f target
  where
    target = minimumBy (comparing f) [minimum input .. maximum input]
    f n = sum $ fmap (cost . abs . subtract n) input

day7 :: Spec
day7 = do
  exam <- runIO $ parseFile "input/2021/07.ex.txt" parser
  input <- runIO $ parseFile "input/2021/07.txt" parser
  star1ex 37 $ ex1 exam
  star1 359648 $ ex1 input
  star2ex 168 $ ex2 exam
  star2 100727924 $ ex2 input
