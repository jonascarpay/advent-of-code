{-# LANGUAGE DeriveTraversable #-}

module Block where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void
import Data.Word
import Parse
import Text.Megaparsec

data Block a = Block
  { bWidth :: Int
  , bHeight :: Int
  , bData :: Vector a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data BlockError

pLine :: Parser ByteString
pLine = takeWhileP Nothing (/= ascii '\n') <* single (ascii '\n')

mkBlock :: Int -> Int -> [a] -> Block a
mkBlock w h as
  | w <= 0 || h <= 0 = error "mkBlock: negative dimension"
  | length as < w * h = error "mkBlock: too few elements"
  | otherwise = Block w h $ V.fromListN (w * h) as

bIndexWrap :: Int -> Int -> Block a -> a
bIndexWrap x y (Block w h vec) = vec V.! (mod x w + mod y h * w)

inBounds :: Int -> Int -> Block a -> Bool
inBounds x y (Block w h _) = x < w && y < h

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

pBlock :: Parser (Block Word8)
pBlock = mkArray <$> pBlockLines
 where
  mkArray (w, h, bss) = mkBlock w h (bss >>= BS.unpack)
  pBlockLines :: Parser (Int, Int, [ByteString])
  pBlockLines = do
    h <- pLine
    let n = BS.length h
    t <- many $ pSuchThat pLine (\l -> if BS.length l == n then pure l else Left empty)
    pure (n, 1 + length t, h : t)
