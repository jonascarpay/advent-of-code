{-# LANGUAGE OverloadedStrings #-}

module AOC2020.Day02 (day2) where

import AOC2020.Common
import Data.Text qualified as T
import Parse
import Test.Hspec

day2 :: Spec
day2 = do
  input <- runIO $ parseFile "input/2020/day2.txt" $ pLines parsePw
  star1 560 $ length $ filter validPass input
  star2 303 $ length $ filter validPass2 input

parsePw :: Parser (Int, Int, Char, Text)
parsePw = do
  lo <- decimal
  chunk "-"
  hi <- decimal
  chunk " "
  c <- anySingle
  chunk ": "
  pw <- T.pack <$> many alphaNumChar
  pure (lo, hi, c, pw)

validPass :: (Int, Int, Char, Text) -> Bool
validPass (lo, hi, c, str) = let n = T.length (T.filter (== c) str) in n >= lo && n <= hi

validPass2 :: (Int, Int, Char, Text) -> Bool
validPass2 (lo, hi, c, str) = do
  let clo = T.index str (lo -1)
      chi = T.index str (hi -1)
   in xor (clo == c) (chi == c)
 where
  xor True False = True
  xor False True = True
  xor _ _ = False
