{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Applicative hiding (many)
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char
import Data.Void
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer qualified as Lex

type Parser = Parsec Void ByteString

word :: Parser ByteString
word = takeWhileP Nothing (\c -> c >= ascii 'a' && c <= ascii 'z')

data Bag = Bag ByteString ByteString
  deriving (Eq, Ord, Show)

parseBags :: Parser [(Bag, [(Int, Bag)])]
parseBags = many (pContains <* eol) <* takeRest
 where
  pBag :: Parser Bag
  pBag = do
    w1 <- word <* chunk " "
    w2 <- word <* chunk " "
    chunk "bags" <|> chunk "bag"
    pure (Bag w1 w2)
  pContains :: Parser (Bag, [(Int, Bag)])
  pContains = do
    bHead <- pBag <* chunk " contain "
    let nDec = Lex.decimal <* chunk " "
    bs <- ([] <$ chunk "no other bags") <|> (sepBy (liftA2 (,) nDec pBag) (chunk ", "))
    chunk "."
    pure $ (bHead, bs)

parseFile :: FilePath -> Parser a -> IO a
parseFile fp p = do
  bs <- BS.readFile fp
  either (fail . errorBundlePretty) pure $ runParser p fp bs

ascii :: Char -> Word8
ascii = fromIntegral . ord

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

pLine :: Parser ByteString
pLine = takeWhileP Nothing (/= ascii '\n') <* single (ascii '\n')
