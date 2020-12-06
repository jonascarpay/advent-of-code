{-# LANGUAGE DeriveTraversable #-}

module Block where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word
import Parse
import Text.Megaparsec

-- 00 01 02 03
-- 10 11 12 13
-- 20 21 22 23
-- 30 31 32 33

data Block a = Block
  { bWidth :: Int
  , bHeight :: Int
  , bData :: Vector a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

blockI :: Block Char
blockI =
  mkBlock 3 4 $
    "###"
      <> " # "
      <> " # "
      <> "###"

blockI' :: Block Char
blockI' =
  mkBlock 18 5 $
    "          ###     "
      <> "  ###      #      "
      <> "   #       #      "
      <> "   #      ###     "
      <> "  ###             "

mkBlock :: Int -> Int -> [a] -> Block a
mkBlock w h as
  | w <= 0 || h <= 0 = error "mkBlock: non-positive dimension"
  | length as < w * h = error "mkBlock: too few elements"
  | otherwise = Block w h $ V.fromListN (w * h) as

slice :: Int -> Int -> Int -> Int -> Block a -> Maybe (Block a)
slice x0 y0 w h b = fmap (mkBlock w h) $
  sequence $ do
    y <- take h [y0 ..]
    x <- take w [x0 ..]
    pure $ bIndex x y b

findBlock :: Eq a => Block a -> Block a -> [(Int, Int)]
findBlock needle haystack =
  [ (x, y)
  | (x, y) <- positions
  , slice x y (bWidth needle) (bHeight needle) haystack == Just needle
  ]
 where
  Just p = bIndex 0 0 needle
  w = bWidth haystack
  offsets = V.toList $ V.findIndices (== p) (bData haystack)
  positions = fmap (\o -> (mod o w, div o w)) offsets

bIndexWrap :: Int -> Int -> Block a -> a
bIndexWrap x y (Block w h vec) = vec V.! (mod x w + mod y h * w)

bIndex :: Int -> Int -> Block a -> Maybe a
bIndex x y b@(Block w _ vec)
  | inBounds x y b = pure $ V.unsafeIndex vec (x + y * w)
  | otherwise = Nothing

inBounds :: Int -> Int -> Block a -> Bool
inBounds x y (Block w h _) = x >= 0 && y >= 0 && x < w && y < h

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
