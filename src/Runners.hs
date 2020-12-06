{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Runners where

import Block
import Control.Monad
import Data.List.Split
import Data.Set qualified as S
import Lib
import Linear
import Parse
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
  block <- parseFile "input/day3.txt" pBlock
  print $ tobbogan 3 1 block
  print $
    product
      [ tobbogan 1 1 block
      , tobbogan 3 1 block
      , tobbogan 5 1 block
      , tobbogan 7 1 block
      , tobbogan 1 2 block
      ]

day4 :: IO ()
day4 = do
  pps <- parseFile "input/day4.txt" (parsePP <* eof)
  print $ length $ filter (\pp -> all (uncurry validField) pp && validpp pp) pps

day5 :: IO ()
day5 = do
  ls <- lines <$> readFile "input/day5.txt"
  print $ maximum $ fmap parseL ls
  print $ missing ls

day6 :: IO ()
day6 = do
  gs <- filter (not . null) . splitOn [""] . lines <$> readFile "input/day6.txt"
  print $ length $ [() | g <- gs, c <- ['a' .. 'z'], all (elem c) g]
  print $ length $ [() | g <- gs, c <- ['a' .. 'z'], any (elem c) g]
