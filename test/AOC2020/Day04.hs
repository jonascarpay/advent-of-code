{-# LANGUAGE OverloadedStrings #-}

module AOC2020.Day04 (day4) where

import AOC2020.Common
import Control.Applicative hiding (many)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Debug.Trace
import Parse
import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer qualified as Lex

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

day4 :: Spec
day4 = do
  pps <- runIO $ parseFile "input/day4.txt" (parsePP <* eof)
  it "star 1" $ expectationFailure "MISSING"
  star2 127 $ length $ filter (\pp -> all (uncurry validField) pp && validpp pp) pps
