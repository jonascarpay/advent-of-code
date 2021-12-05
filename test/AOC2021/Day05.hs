module AOC2021.Day05 where

import Control.Monad
import Data.Histogram qualified as H
import Lib
import Linear
import Parse
import Test.Hspec
import TestLib

type Input = [(V2 Int, V2 Int)]

parser :: Parser Input
parser = pLines $ do
  [x1, y1, x2, y2] <- replicateM 4 $ next decimal
  pure (V2 x1 y1, V2 x2 y2)

ex1 :: Input -> Int
ex1 ps = length $ filter ((> 1) . snd) $ H.toList $ H.fromList $ ps >>= uncurry line
  where
    line (V2 x1 y1) (V2 x2 y2)
      | x1 == x2 = V2 x1 <$> range y1 y2
      | y1 == y2 = flip V2 y1 <$> range x1 x2
      | otherwise = []

ex2 :: Input -> Int
ex2 ps = length $ filter ((> 1) . snd) $ H.toList $ H.fromList $ ps >>= uncurry line
  where
    line (V2 x1 y1) (V2 x2 y2)
      | x1 == x2 = V2 x1 <$> range y1 y2
      | y1 == y2 = flip V2 y1 <$> range x1 x2
      | otherwise = zipWith V2 (range x1 x2) (range y1 y2)

day5 :: Spec
day5 = do
  exam <- runIO $ parseFile "input/2021/05.ex.txt" parser
  input <- runIO $ parseFile "input/2021/05.txt" parser
  star1ex 5 $ ex1 exam
  star1 6225 $ ex1 input
  star2ex 12 $ ex2 exam
  star2 22116 $ ex2 input
