module Parse where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char
import Data.Void
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte

type Parser = Parsec Void ByteString

parseFile :: FilePath -> Parser a -> IO a
parseFile fp p = do
  bs <- BS.readFile fp
  either (fail . errorBundlePretty) pure $ runParser p fp bs

ascii :: Char -> Word8
ascii = fromIntegral . ord

pLines :: Parser a -> Parser [a]
pLines p = many (p <* eol) <* eof
