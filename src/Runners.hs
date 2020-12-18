{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-imports #-}

module Runners where

import Block
import Control.Monad
import Data.Bits
import Data.Bool
import Data.Char
import Data.Either
import Data.Foldable (toList)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Sequence qualified as Q
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug.Trace
import Lib
import Linear hiding (E)
import Parse

step :: Set (V3 Int) -> Set (V3 Int)
step s = S.fromList $ filter mem vs
 where
  f1 (V3 x _ _) = x
  f2 (V3 _ x _) = x
  f3 (V3 _ _ x) = x
  vs :: [V3 Int]
  vs = [V3 x y z | x <- [xn - 1 .. xm + 1], y <- [yn -1 .. ym + 1], z <- [zn -1 .. zm + 1]]
  xn, xm, yn, ym, zn, zm :: Int
  xn = S.findMin $ S.map f1 s
  xm = S.findMax $ S.map f1 s
  yn = S.findMin $ S.map f2 s
  ym = S.findMax $ S.map f2 s
  zn = S.findMin $ S.map f3 s
  zm = S.findMax $ S.map f2 s
  mem :: V3 Int -> Bool
  mem v =
    let ns = do
          x <- [-1, 0, 1]
          y <- [-1, 0, 1]
          z <- [-1, 0, 1]
          let d = V3 x y z
          guard (d /= 0)
          guard $ S.member (v + d) s
          pure ()
        n = length ns
     in if S.member v s then n == 2 || n == 3 else n == 3

step2 :: Set (V4 Int) -> Set (V4 Int)
step2 s = S.fromList $ filter mem vs
 where
  f1 (V4 x _ _ _) = x
  f2 (V4 _ x _ _) = x
  f3 (V4 _ _ x _) = x
  f4 (V4 _ _ _ x) = x
  vs :: [V4 Int]
  vs = [V4 x y z w | x <- [xn - 1 .. xm + 1], y <- [yn -1 .. ym + 1], z <- [zn -1 .. zm + 1], w <- [wn -1 .. wm + 1]]
  xn, xm, yn, ym, zn, zm :: Int
  xn = S.findMin $ S.map f1 s
  xm = S.findMax $ S.map f1 s
  yn = S.findMin $ S.map f2 s
  ym = S.findMax $ S.map f2 s
  zn = S.findMin $ S.map f3 s
  zm = S.findMax $ S.map f3 s
  wn = S.findMin $ S.map f4 s
  wm = S.findMax $ S.map f4 s
  mem :: V4 Int -> Bool
  mem v =
    let ns = do
          x <- [-1, 0, 1]
          y <- [-1, 0, 1]
          z <- [-1, 0, 1]
          w <- [-1, 0, 1]
          let d = V4 x y z w
          guard (d /= 0)
          guard $ S.member (v + d) s
          pure ()
        n = length ns
     in if S.member v s then n == 2 || n == 3 else n == 3

day17 :: IO ()
day17 = do
  p <- parseFile "input/day17.txt" pBlock
  let p' = imap (\(x, y) a -> (V3 x y 0, a)) p
      l = S.fromList $ map fst $ filter ((== '#') . snd) $ bList p'
      ss = iterate step l
  print $ length $ ss !! 6
  let p' = imap (\(x, y) a -> (V4 x y 0 0, a)) p
      l = S.fromList $ map fst $ filter ((== '#') . snd) $ bList p'
      ss = iterate step2 l
  print $ length $ ss !! 6
