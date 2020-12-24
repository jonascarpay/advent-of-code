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
import Linear hiding (E, rotate, trace)
import Parse hiding (State)

data Dir
  = E
  | SE
  | SW
  | W
  | NW
  | NE
  deriving (Show, Enum)

parse24 :: Parser [[Dir]]
parse24 =
  sepBy
    ( many $
        choice
          [ E <$ chunk "e",
            W <$ chunk "w",
            SE <$ chunk "se",
            SW <$ chunk "sw",
            NE <$ chunk "ne",
            NW <$ chunk "nw"
          ]
    )
    eol

toC :: Dir -> V3 Int
toC NE = V3 1 (-1) 0
toC E = V3 1 0 (-1)
toC SE = V3 0 1 (-1)
toC SW = V3 (-1) 1 0
toC W = V3 (-1) 0 1
toC NW = V3 0 (-1) 1

haxa :: V3 Int -> [V3 Int]
haxa v = (v +) . toC <$> [E .. NE]

toggle :: Ord a => a -> Set a -> Set a
toggle a s = if S.member a s then S.delete a s else S.insert a s

step :: Set (V3 Int) -> Set (V3 Int)
step s = S.fromList $ filter next all
  where
    all :: [V3 Int]
    all = S.toList s >>= haxa
    next :: V3 Int -> Bool
    next v =
      let n = length $ filter (`S.member` s) (haxa v)
          t = S.member v s
       in if t
            then not $ n == 0 || n > 2
            else n == 2

day24 :: IO ()
day24 = do
  p <- filter (not . null) <$> parseFile "input/day24.txt" parse24
  -- let ts = s1 mempty 0 <$> p
  let coords = sum . fmap toC <$> p
      s0 = S.fromList $ fmap fst $ filter (odd . snd) $ H.toList $ H.fromList coords
  print $ length $ filter (odd . snd) $ H.toList $ H.fromList coords
  print $ (S.size <$> iterate step s0) !! 100
