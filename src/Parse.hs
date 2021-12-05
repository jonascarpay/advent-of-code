{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( module Parse,
    module Text.Megaparsec,
    module Text.Megaparsec.Char,
    Text,
  )
where

import Control.Monad
import Data.Char
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lex

type Parser = Parsec Void Text

decimal :: Parser Int
decimal = Lex.decimal

skip :: Int -> Parser ()
skip n = void $ takeP Nothing n

word :: Parser Text
word = takeWhile1P Nothing isAsciiLower

parseFile :: FilePath -> Parser a -> IO a
parseFile fp p = do
  bs <- T.readFile fp
  either (fail . errorBundlePretty) pure $ runParser (p <* takeRest) fp bs

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
pLine = takeWhileP Nothing (/= '\n') <* single '\n'

next :: Parser a -> Parser a
next = skipManyTill anySingle

pSpace :: Parser ()
pSpace = Lex.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = (<* pSpace)
