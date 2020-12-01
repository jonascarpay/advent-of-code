{-# LANGUAGE TypeApplications #-}

import Lib
import Linear

main :: IO ()
main = do
  input <- readListOfInts "input/day1.txt"
  print
    . fmap product
    . filter ((== 2020) . sum)
    . uniqueTuples @V2
    $ input
  print
    . fmap product
    . filter ((== 2020) . sum)
    . uniqueTuples @V3
    $ input
