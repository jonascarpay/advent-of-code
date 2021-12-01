{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day20 (day20) where

import Block
import Data.Foldable (toList)
import Data.Histogram qualified as H
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Vector (Vector)
import Data.Vector qualified as V
import Lib
import Linear hiding (E, rotate, trace)
import Parse hiding (State)
import TestLib

ptile :: Parser [(Int, Block Char)]
ptile = sepBy m eol
  where
    m = do
      chunk "Tile "
      n <- decimal
      chunk ":"
      eol
      b <- pBlock
      pure (n, b)

edges :: Block a -> [V.Vector a]
edges b@(Block (V2 w h) _) = [topV b, leftV b, rightV b, bottomV b]

norme :: Ord a => V.Vector a -> V.Vector a
norme v = min v (V.reverse v)

perms :: Block a -> [Block a]
perms b0 =
  [ b3
    | b1 <- [b0, reflectX b0],
      b2 <- [b1, reflectY b1],
      b3 <- bRotates b2
  ]

jigsaw :: forall a. Ord a => [Block a] -> Map (V2 Int) (Block a)
jigsaw bs = go mempty [0] bs'
  where
    bs' :: [Block a]
    bs' = bs >>= perms
    go :: Map (V2 Int) (Block a) -> [V2 Int] -> [Block a] -> Map (V2 Int) (Block a)
    go m [] bs = m
    go m (v : vs) bs | M.member v m = go m vs bs
    go m (v : vs) bs =
      case break (fits m v) bs of
        (p, b : q) -> go (M.insert v b m) (orthogonal v <> vs) (filter (`notElem` perms b) $ p <> q)
        _ -> go m vs bs
    fits :: Map (V2 Int) (Block a) -> V2 Int -> Block a -> Bool
    fits m va ba =
      let bbs = [(bb, vb) | vb <- orthogonal va, bb <- toList (M.lookup vb m)]
       in all (\(bb, vb) -> fit ba (va - vb) bb) bbs
    fit :: Block a -> V2 Int -> Block a -> Bool
    fit a (V2 0 1) b = topV a == bottomV b
    fit a (V2 0 (-1)) b = bottomV a == topV b
    fit a (V2 (-1) 0) b = rightV a == leftV b
    fit a (V2 1 0) b = leftV a == rightV b
    fit _ v _ = error $ "broken relative position " <> show v

assemble :: Map (V2 Int) (Block a) -> Block a
assemble m = generate (s * (vmax + 1)) $
  \v -> let (vm, vb) = vdivmod v s in fromJust $ bIndex vb (m' M.! vm)
  where
    vdivmod (V2 x y) (V2 w h) =
      let (xd, xm) = divMod x w
          (yd, ym) = divMod y h
       in (V2 xd yd, V2 xm ym)
    Block s _ = m M.! vOff
    vmax = maximum $ M.keys m'
    m' = M.mapKeys (subtract vOff) m
    vOff = minimum $ M.keys m

ex1 :: [(Int, Block Char)] -> Int
ex1 input =
  let edgeCounts :: H.Histogram (Vector Char)
      edgeCounts = H.fromList $ input >>= fmap norme . edges . snd
      isCorner b = count (\e -> H.lookup e edgeCounts == 1) (norme <$> edges b) == 2
   in product . fmap fst . filter (isCorner . snd) $ input

ex2 :: [(Int, Block Char)] -> Int
ex2 bs = total - length vs * 15
  where
    total = V.length $ V.filter (== '#') $ bData final
    (vs, final) = case withMonsters of
      [(v, f), _] -> (v, f)
      x -> error (show x)
    withMonsters :: [([V2 Int], Block Char)]
    withMonsters = filter (not . null . fst) $ (\b -> (findMonsters b, b)) <$> perms block
    findMonsters :: Block Char -> [V2 Int]
    findMonsters = findBlockWith f monster
      where
        f '#' '#' = True
        f '#' _ = False
        f _ _ = True
    map :: Map (V2 Int) (Block Char)
    map = jigsaw (snd <$> bs)
    block :: Block Char
    block = assemble $ trimEdges <$> map
    trimEdges :: Block Char -> Block Char
    trimEdges b = fromJust $ slice 1 (bSize b - 2) b

monster :: Block Char
monster =
  mkBlock (V2 20 3) $
    concat
      [ "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   "
      ]

day20 :: Spec
day20 = do
  pex <- runIO $ parseFile "input/2020/day20ex.txt" ptile
  p <- runIO $ parseFile "input/2020/day20.txt" ptile
  star1ex 20899048083289 $ ex1 pex
  star1 22878471088273 $ ex1 p
  star2ex 273 $ ex2 pex
  star2 1680 $ ex2 p
