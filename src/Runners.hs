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
import Data.Bits
import Data.Bool
import Data.Char
import Data.Either
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
import Data.Vector qualified as V
import Debug.Trace
import Histogram qualified as H
import Lib
import Linear hiding (E)
import Parse

parse22 :: Parser ([Int], [Int])
parse22 = do
  takeWhileP Nothing (/= '\n') <* eol
  p1 <- many $ decimal <* eol
  eol
  takeWhileP Nothing (/= '\n') <* eol
  p2 <- many $ decimal <* eol
  pure (p1, p2)

play [] xs = Right xs
play xs [] = Left xs
play (l : ls) (r : rs) = case compare l r of
  LT -> play ls (rs <> [r, l])
  GT -> play (ls <> [l, r]) rs
  _ -> undefined

type Hand = Seq Int

type Memo = Map (Hand, Hand) (Either Hand Hand)

playround :: Set (Hand, Hand) -> Memo -> Hand -> Hand -> (Memo, Either Hand Hand)
playround c1 s h1 h2
  | Seq.null h1 = (M.insert (h1, h2) (Right h2) s, Right h2)
  | Seq.null h2 = (M.insert (h1, h2) (Left h1) s, Left h1)
  | S.member (h1, h2) c1 = (M.insert (h1, h2) (Right h1) s, Right h1)
  | M.member (h1, h2) s = (s, s M.! (h1, h2))
playround c1 s h1@(l :<| ls) h2@(r :<| rs)
  | length ls >= l && length rs >= r = case playround mempty s ls rs of
    (s', Right res) -> playround (S.insert (h1, h2) c1) (M.insert (h1, h2) (Right res) s') ls (rs :|> r :|> l)
    (s', Left res) -> playround (S.insert (h1, h2) c1) (M.insert (h1, h2) (Left res) s') (ls :|> l :|> r) rs
playround c1 s h1@(l :<| ls) h2@(r :<| rs) =
  case compare l r of
    LT -> playround (S.insert (h1, h2) c1) s ls (rs :|> r :|> l)
    GT -> playround (S.insert (h1, h2) c1) s (ls :|> l :|> r) rs
    _ -> undefined

f (Left x) = x
f (Right x) = x

day22 :: IO ()
day22 = do
  (l, r) <- parseFile "input/day22ex.txt" parse22
  -- input <- lines <$> readFile "input/day22.txt"
  -- let final = f $ play l r
  -- print $ sum $ zipWith (*) (reverse final) [1 ..]
  let (_, x) = playround mempty mempty (Seq.fromList l) (Seq.fromList r)
  let (final) = toList $ f $ x
  print $ sum $ zipWith (*) (reverse final) [1 ..]

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
