module AOC2020.Day25 (day25) where

import AOC2020.Common
import Data.List (elemIndex)
import Data.Maybe (fromJust)

s1 :: (Int, Int) -> Int
s1 (cp, dp) = iterate (transform cp) 1 !! (fromJust . elemIndex dp . iterate (transform 7)) 1

transform :: Int -> Int -> Int
transform subj n = mod (n * subj) 20201227

day25 :: Spec
day25 = do
  let iex = (5764801, 17807724)
  let i = (15733400, 6408062)
  star1 14897079 $ s1 iex
  star1 16457981 $ s1 i
