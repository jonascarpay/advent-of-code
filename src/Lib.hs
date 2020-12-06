{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Block
import Control.Applicative hiding (many)
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntSet qualified as IS
import Data.Word
import Debug.Trace
import Parse
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer qualified as Lex

missing :: [String] -> [Int]
missing str = [x | x <- [0 .. 1000], IS.member (x -1) present && IS.member (x + 1) present && not (IS.member x present)]
 where
  present = IS.fromList $ parseL <$> str

parseL :: String -> Int
parseL = go 0
 where
  go n ('F' : t) = go (2 * n) t
  go n ('B' : t) = go (2 * n + 1) t
  go n ('L' : t) = go (2 * n) t
  go n ('R' : t) = go (2 * n + 1) t
  go n [] = n

parsePP :: Parser [[(ByteString, ByteString)]]
parsePP = sepBy1 entry eol
 where
  entry :: Parser [(ByteString, ByteString)]
  entry = many field -- sepBy1 field (eol <|> chunk " ")
  field :: Parser (ByteString, ByteString)
  field = do
    k <- BS.pack <$> many (single (ascii '#') <|> alphaNumChar)
    chunk ":"
    v <- BS.pack <$> many (single (ascii '#') <|> alphaNumChar)
    chunk " " <|> eol
    pure (k, v)

validpp :: [(ByteString, ByteString)] -> Bool
validpp ss = case lookup "cid" ss of
  Nothing -> length ss == 7
  Just _ -> length ss == 8

validField :: ByteString -> ByteString -> Bool
validField "byr" bs = case runParser (Lex.decimal <* eof :: Parser Int) "byr" bs of
  Right n -> n >= 1920 && n <= 2002
  Left err -> traceShow err $ False
validField "iyr" bs = case runParser (Lex.decimal <* eof :: Parser Int) "iyr" bs of
  Right n -> n >= 2010 && n <= 2020
  _ -> False
validField "eyr" bs = case runParser (Lex.decimal <* eof :: Parser Int) "eyr" bs of
  Right n -> n >= 2020 && n <= 2030
  _ -> False
validField "hgt" bs = case runParser ((,) <$> Lex.decimal <*> (chunk "cm" <|> chunk "in") <* eof :: Parser (Int, ByteString)) "eyr" bs of
  Right (n, "in") -> n >= 59 && n <= 76
  Right (n, "cm") -> n >= 150 && n <= 193
  _ -> False
validField "hcl" bs = case runParser (chunk "#" *> takeP Nothing 6 <* eof :: Parser ByteString) "hcl" bs of
  Right n -> BS.all (\w -> (w >= ascii '0' && w <= ascii '9') || (w >= ascii 'a' && w <= ascii 'f')) n
  _ -> False
validField "ecl" bs = case runParser (takeP Nothing 3 <* eof :: Parser ByteString) "ecl" bs of
  Right n -> n `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  _ -> False
validField "pid" bs = case runParser (takeP Nothing 9 <* eof :: Parser ByteString) "pid" bs of
  Right n -> BS.all (\w -> w >= ascii '0' && w <= ascii '9') n
  _ -> False
validField "cid" bs = True
validField _ bs = False

-- Day 3

tobbogan :: Int -> Int -> Block Word8 -> Int
tobbogan dx dy b = length $ filter (== ascii '#') $ go 0 0
 where
  go :: Int -> Int -> [Word8]
  go x y
    | y < bHeight b = bIndexWrap x y b : go (x + dx) (y + dy)
    | otherwise = []

-- Day 2

parsePw :: Parser (Int, Int, Word8, ByteString)
parsePw = do
  lo <- Lex.decimal
  chunk "-"
  hi <- Lex.decimal
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

-- Day 1

uniques :: Traversable t => t () -> [a] -> [t a]
uniques base as =
  flip evalStateT as $
    forM base $
      const $
        fix $ \f ->
          get >>= \case
            [] -> empty
            (h : t) -> put t >> (pure h <|> f)

uniqueTuples :: (Traversable t, Applicative t) => [a] -> [t a]
uniqueTuples = uniques (pure ())

readListOfInts :: FilePath -> IO [Int]
readListOfInts = fmap parseInts . readFile
 where
  parseInts :: String -> [Int]
  parseInts = fmap read . lines
