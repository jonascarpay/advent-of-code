{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Control.Applicative hiding (many)
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Void
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer qualified as Lex

type Parser = Parsec Void BS.ByteString

parseFile :: FilePath -> Parser a -> IO a
parseFile fp p = do
  bs <- BS.readFile fp
  either (fail . errorBundlePretty) pure $ runParser p fp bs

pLines :: Parser a -> Parser [a]
pLines p = many (p <* eol) <* eof

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
