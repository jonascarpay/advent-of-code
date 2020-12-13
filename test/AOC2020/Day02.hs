{-# LANGUAGE OverloadedStrings #-}

module AOC2020.Day02 (day2) where

import AOC2020.Common
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word
import Parse
import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Byte

day2 :: Spec
day2 = do
  input <- runIO $ parseFile "input/day2.txt" $ pLines parsePw
  star1 560 $ length $ filter validPass input
  star2 303 $ length $ filter validPass2 input

parsePw :: Parser (Int, Int, Word8, ByteString)
parsePw = do
  lo <- decimal
  chunk "-"
  hi <- decimal
  chunk " "
  c <- anySingle
  chunk ": "
  pw <- BS.pack <$> many alphaNumChar
  pure (lo, hi, c, pw)

validPass :: (Int, Int, Word8, ByteString) -> Bool
validPass (lo, hi, c, str) = let n = BS.length (BS.filter (== c) str) in n >= lo && n <= hi

validPass2 :: (Int, Int, Word8, ByteString) -> Bool
validPass2 (lo, hi, c, str) = do
  let clo = BS.index str (lo -1)
      chi = BS.index str (hi -1)
   in xor (clo == c) (chi == c)
 where
  xor True False = True
  xor False True = True
  xor _ _ = False
