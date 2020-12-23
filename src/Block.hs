{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Block where

import Control.Applicative
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Linear
import Parse hiding (many)

-- 00 01 02 03
-- 10 11 12 13
-- 20 21 22 23
-- 30 31 32 33

data Block a = Block
  { bSize :: V2 Int,
    bData :: Vector a
  }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

blockI :: Block Char
blockI =
  mkBlock (V2 3 4) $
    concat
      [ "###",
        " # ",
        " # ",
        "###"
      ]

blockI' :: Block Char
blockI' =
  mkBlock (V2 18 5) $
    concat
      [ "          ###     ",
        "  ###      #      ",
        "   #       #      ",
        "   #      ###     ",
        "  ###             "
      ]

mkBlock :: V2 Int -> [a] -> Block a
mkBlock v@(V2 w h) as
  | w <= 0 || h <= 0 = error "mkBlock: non-positive dimension"
  | length as < product v = error "mkBlock: too few elements"
  | otherwise = Block v $ V.fromListN (product v) as

slice :: V2 Int -> V2 Int -> Block a -> Maybe (Block a)
slice (V2 x0 y0) s@(V2 w h) b = fmap (mkBlock s) $
  sequence $ do
    y <- take h [y0 ..]
    x <- take w [x0 ..]
    pure $ bIndex (V2 x y) b

iToV :: V2 Int -> Int -> V2 Int
iToV (V2 w _) i = let (y, x) = divMod i w in V2 x y

vToI :: V2 Int -> V2 Int -> Int
vToI (V2 w _) (V2 x y) = y * w + x

findBlock :: Eq a => Block a -> Block a -> [V2 Int]
findBlock needle haystack =
  [ v
    | v <- positions,
      slice v (bSize needle) haystack == Just needle
  ]
  where
    p = bIndexWrap 0 needle
    offsets = V.toList $ V.findIndices (== p) (bData haystack)
    positions = fmap (iToV (bSize haystack)) offsets

slices :: V2 Int -> V2 Int -> [V2 Int]
slices needle haystack = sequence $ liftA2 (\n h -> take (h - n + 1) [0 ..]) needle haystack

findBlockWith :: (a -> b -> Bool) -> Block a -> Block b -> [V2 Int]
findBlockWith f needle haystack =
  [ v
    | v <- slices (bSize needle) (bSize haystack),
      let Just s = slice v (bSize needle) haystack
       in V.and (V.zipWith f (bData needle) (bData s))
  ]

imap :: (V2 Int -> a -> b) -> Block a -> Block b
imap f (Block s arr) = Block s (V.imap (f . iToV s) arr)

bList :: Block a -> [a]
bList (Block _ v) = V.toList v

bIndexWrap :: V2 Int -> Block a -> a
bIndexWrap (V2 x y) (Block (V2 w h) vec) = vec V.! (mod x w + mod y h * w)

bIndex :: V2 Int -> Block a -> Maybe a
bIndex v b@(Block s vec)
  | inBounds v b = pure $ V.unsafeIndex vec (vToI s v)
  | otherwise = Nothing

reflectX :: Block a -> Block a
reflectX b = generate (bSize b) $ \(V2 x y) -> bIndexWrap (V2 (- x -1) y) b

reflectY :: Block a -> Block a
reflectY b = generate (bSize b) $ \(V2 x y) -> bIndexWrap (V2 x (- y -1)) b

bRotate :: Block a -> Block a
bRotate b@(Block (V2 w h) _) = generate (V2 h w) $ \(V2 x y) -> bIndexWrap (V2 (- y -1) x) b

bRotates :: Block a -> [Block a]
bRotates = take 4 . iterate bRotate

generate :: V2 Int -> (V2 Int -> a) -> Block a
generate s f = Block s $ V.generate (product s) $ f . iToV s

inBounds :: V2 Int -> Block a -> Bool
inBounds (V2 x y) (Block (V2 w h) _) = x >= 0 && y >= 0 && x < w && y < h

showBlock :: forall a. (a -> Char) -> Block a -> String
showBlock f (Block (V2 w _) v) = unlines $ go (V.toList v)
  where
    go :: [a] -> [String]
    go [] = []
    go ls = let (h, t) = splitAt w ls in (f <$> h) : go t

pBlock :: Parser (Block Char)
pBlock = mkArray <$> pBlockLines
  where
    mkArray (w, h, bss) = mkBlock (V2 w h) (bss >>= T.unpack)
    pBlockLines :: Parser (Int, Int, [Text])
    pBlockLines = do
      h <- pLine
      let n = T.length h
      t <- many $ pSuchThat pLine (\l -> if T.length l == n then pure l else Left empty)
      pure (n, 1 + length t, h : t)

topV, leftV, rightV, bottomV :: Block a -> V.Vector a
topV (Block (V2 w _) v) = V.take w v
leftV (Block (V2 w h) v) = V.generate h (\i -> v V.! (w * i))
rightV (Block (V2 w h) v) = V.generate h (\i -> v V.! (w * (i + 1) - 1))
bottomV (Block (V2 w h) v) = V.generate h (\i -> v V.! ((h -1) * w + i))
