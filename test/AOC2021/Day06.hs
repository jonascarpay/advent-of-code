module AOC2021.Day06 where

import Data.Histogram qualified as H
import Lib
import Parse
import Test.Hspec
import TestLib

type Input = [Int]

parser :: Parser Input
parser = decimal `sepBy` char ','

fish :: Int -> [Int] -> Int
fish days input = sum $ fmap snd $ H.toList $ nTimes days (H.flatMap f) $ H.fromList input
  where
    f 0 n = H.fromCountList [(6, n), (8, n)]
    f i n = H.singletonCount (i - 1) n

day6 :: Spec
day6 = do
  exam <- runIO $ parseFile "input/2021/06.ex.txt" parser
  input <- runIO $ parseFile "input/2021/06.txt" parser
  star1ex 5 $ fish 0 exam
  star1ex 26 $ fish 18 exam
  star1ex 5934 $ fish 80 exam
  star1 345387 $ fish 80 input
  star2ex 26984457539 $ fish 256 exam
  star2 1574445493136 $ fish 256 input
