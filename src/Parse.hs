{-# LANGUAGE OverloadedStrings #-}

module Parse (
  module Parse,
  module Text.Megaparsec,
  module Text.Megaparsec.Char,
  Text,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lex

type Parser = Parsec Void Text

decimal :: Parser Int
decimal = Lex.decimal

word :: Parser Text
word = takeWhileP Nothing (\c -> c >= 'a' && c <= 'z')

parseFile :: FilePath -> Parser a -> IO a
parseFile fp p = do
  bs <- T.readFile fp
  either (fail . errorBundlePretty) pure $ runParser p fp bs

pLines :: Parser a -> Parser [a]
pLines p = many (p <* eol) <* eof

{-# INLINE pSuchThat #-}
pSuchThat :: MonadParsec e s m => m a -> (a -> Either (m Void) b) -> m b
pSuchThat m f = do
  (n, a) <- lookAhead $ do
    p <- getOffset
    a <- m
    q <- getOffset
    pure (q - p, a)
  case f a of
    Left err -> absurd <$> err
    Right b -> b <$ takeP Nothing n

pLine :: Parser Text
pLine = takeWhileP Nothing (/= '\n') <* single ('\n')
