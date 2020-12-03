{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Runners where

import Data.ByteString qualified as BS
import Lib
import Linear
import Text.Megaparsec
import Text.Megaparsec.Byte.Lexer qualified as Lex

day1 :: IO ()
day1 = do
  input :: [Int] <- parseFile "input/day1.txt" $ pLines Lex.decimal
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

day2 :: IO ()
day2 = do
  input <- parseFile "input/day2.txt" $ pLines parsePw
  print $ length $ filter validPass input
  print $ length $ filter validPass2 input

day3 :: IO ()
day3 = do
  input <- BS.readFile "input/day3.txt"
  print $ tobbogan 3 1 input
  print $
    product
      [ tobbogan 1 1 input
      , tobbogan 3 1 input
      , tobbogan 5 1 input
      , tobbogan 7 1 input
      , tobbogan 1 2 input
      ]
