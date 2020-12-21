{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC2020.Day17 (day17) where

import AOC2020.Common
import Block
import Control.Applicative
import Data.Set (Set)
import Data.Set qualified as S
import Lib
import Linear hiding (E)
import Parse

step :: (Bounded (v Int), Applicative v, Traversable v, Ord (v Int)) => Set (v Int) -> Set (v Int)
step s = S.fromList $ filter mem vs
 where
  v = S.toList s
  lo = foldr (liftA2 min) maxBound v
  hi = foldr (liftA2 max) minBound v
  vs = sequence $ liftA2 (\l h -> [l -1 .. h + 1]) lo hi
  mem v =
    let n = length $ filter (`S.member` s) (adjacent v)
     in if S.member v s then n == 2 || n == 3 else n == 3

day17 :: Spec
day17 = do
  p <- runIO $ parseFile "input/day17.txt" pBlock
  let p' = imap (\(V2 x y) a -> (V3 x y 0, a)) p
      l = S.fromList $ map fst $ filter ((== '#') . snd) $ bList p'
      ss = iterate step l
   in star1 401 $ length $ ss !! 6
  let p' = imap (\(V2 x y) a -> (V4 x y 0 0, a)) p
      l = S.fromList $ map fst $ filter ((== '#') . snd) $ bList p'
      ss = iterate step l
   in star2 2224 $ length $ ss !! 6
