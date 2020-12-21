{-# LANGUAGE OverloadedStrings #-}

module AOC2020.Day04 (day4) where

import AOC2020.Common
import Control.Applicative hiding (many)
import Data.Text qualified as T
import Debug.Trace
import Parse

validpp :: [(Text, Text)] -> Bool
validpp ss = case lookup "cid" ss of
  Nothing -> length ss == 7
  Just _ -> length ss == 8

validField :: Text -> Text -> Bool
validField "byr" bs = case runParser (decimal <* eof :: Parser Int) "byr" bs of
  Right n -> n >= 1920 && n <= 2002
  Left err -> traceShow err $ False
validField "iyr" bs = case runParser (decimal <* eof :: Parser Int) "iyr" bs of
  Right n -> n >= 2010 && n <= 2020
  _ -> False
validField "eyr" bs = case runParser (decimal <* eof :: Parser Int) "eyr" bs of
  Right n -> n >= 2020 && n <= 2030
  _ -> False
validField "hgt" bs = case runParser ((,) <$> decimal <*> (chunk "cm" <|> chunk "in") <* eof :: Parser (Int, Text)) "eyr" bs of
  Right (n, "in") -> n >= 59 && n <= 76
  Right (n, "cm") -> n >= 150 && n <= 193
  _ -> False
validField "hcl" bs = case runParser (chunk "#" *> takeP Nothing 6 <* eof :: Parser Text) "hcl" bs of
  Right n -> T.all (\w -> (w >= '0' && w <= '9') || (w >= 'a' && w <= 'f')) n
  _ -> False
validField "ecl" bs = case runParser (takeP Nothing 3 <* eof :: Parser Text) "ecl" bs of
  Right n -> n `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  _ -> False
validField "pid" bs = case runParser (takeP Nothing 9 <* eof :: Parser Text) "pid" bs of
  Right n -> T.all (\w -> w >= '0' && w <= '9') n
  _ -> False
validField "cid" _ = True
validField _ _ = False

parsePP :: Parser [[(Text, Text)]]
parsePP = sepBy1 entry eol
 where
  entry :: Parser [(Text, Text)]
  entry = many field -- sepBy1 field (eol <|> chunk " ")
  field :: Parser (Text, Text)
  field = do
    k <- T.pack <$> many (single '#' <|> alphaNumChar)
    chunk ":"
    v <- T.pack <$> many (single '#' <|> alphaNumChar)
    chunk " " <|> eol
    pure (k, v)

day4 :: Spec
day4 = do
  pps <- runIO $ parseFile "input/day4.txt" (parsePP <* eof)
  it "star 1" $ pending
  star2 127 $ length $ filter (\pp -> all (uncurry validField) pp && validpp pp) pps
