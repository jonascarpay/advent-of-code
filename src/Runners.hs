{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-imports #-}

module Runners where

import Block
import Control.Applicative hiding (many, some)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Bits hiding (rotate)
import Data.Bool
import Data.Char
import Data.Either
import Data.Foldable
import Data.Foldable (fold, toList)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Q
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace
import Histogram qualified as H
import Lib
import Linear hiding (E, rotate)
import Parse hiding (State)

exinput :: Vector Int
exinput = V.fromList [3, 8, 9, 1, 2, 5, 4, 6, 7]

input :: Vector Int
input = V.fromList [1, 8, 6, 5, 2, 4, 9, 7, 3]

move :: Int -> Int -> Int -> Vector Int -> Vector Int
move lo hi n v = V.generate n f
  where
    (curr, pick) = let v' = V.take 4 v in (V.head v', V.tail v')
    trgI = go (curr -1)
      where
        go :: Int -> Int
        go n
          | n < lo = go hi
          | V.elem n pick = go (n -1)
          | otherwise = fromJust $ V.elemIndex n v
    f i =
      case () of
        _
          | i + 1 == n -> v V.! 0
          | i < trgI -3 -> v V.! (i + 4)
          | i >= trgI -> v V.! (i + 1)
          | otherwise -> pick V.! (i - trgI + 3)

rotate :: Int -> Vector Int -> Vector Int
rotate m as = let (p, q) = V.splitAt (mod m (V.length as)) as in q <> p

order :: Vector Int -> Vector Int
order t = V.tail $ rotate (fromJust $ V.elemIndex 1 t) t

day23 :: IO ()
day23 = do
  print $
    order $
      let v = input
       in iterate (move (V.minimum v) (V.maximum v) (V.length v)) v !! 100

-- star2 [1] $ take 2 $ order $ iterate move (input <> [10 .. 1000000]) !! 10000000
{--
   -- Day 20 crap
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

edges :: Block a -> [(V.Vector a, V2 Int)]
edges b@(Block (V2 w h) _) =
  [ (topV b, V2 0 1)
  , (leftV b, V2 (-1) 0)
  , (rightV b, V2 1 0)
  , (bottomV b, V2 0 (-1))
  ]

edges' :: Ord a => Block a -> [V.Vector a]
edges' = fmap (norme . fst) . edges

norme :: Ord a => V.Vector a -> V.Vector a
norme v = min v (V.reverse v)

perms :: Block a -> [Block a]
perms b0 =
  [ b3
  | b1 <- [b0, reflectX b0]
  , b2 <- [b1, reflectY b1]
  , b3 <- bRotates b2
  ]

assemble :: forall a. Ord a => [(Int, Block a)] -> Block a
assemble bs = undefined
 where
  hasEdges :: Map (V.Vector a) IntSet
  hasEdges = M.fromListWith (<>) (bs >>= (\(n, b) -> (,IS.singleton n) <$> edges' b))
  isCorner :: Block a -> Bool
  isCorner b = count (\e -> maybe False ((== 1) . IS.size) $ M.lookup e hasEdges) (edges' b) == 2
  corners :: [Block a]
  corners = fmap snd $ filter (\(n, b) -> isCorner b) bs
  p0 = head corners

-- alg : place vertically, place horizontally
-- go :: Map (V2 Int) (Block a) -> [(V.Vector a, V2 Int)] -> Map (V2 Int) (Block a)
-- go m [] = m
-- go m ((e, p) : es)
--   | maybe True ((/= 2) . IS.size) (M.lookup (norme e) hasEdges) = go m es -- edge doesn't connect
--   | M.member p m = go m es -- already placed
--   | otherwise = go (M.insert p fit m) ((fmap . fmap) (+ p) (edges fit) <> es)
--  where
--   [fit] = filter _ (perms $ l)

day20 :: IO ()
day20 = do
  p <- parseFile "input/day20.txt" ptile
  let edgeCounts = H.fromList $ p >>= edges . snd
      isCorner b = count (\e -> H.lookup e edgeCounts == 1) (edges b) == 2
  print $ product $ fmap fst $ filter (isCorner . snd) $ p
  print $ assemble p

-- print $ head p
-- print $ take 4 $ edges $ snd (head p)
--}
