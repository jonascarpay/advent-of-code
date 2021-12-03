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
import Data.Histogram qualified as H
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
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
import Lib
import Linear hiding (E, rotate, trace)
import Parse hiding (State)

f :: String -> Int
f input =
  let go :: [[String]] -> Int -> Int -> Int -> Int
      go (["forward", n] : r) x y a = go r (x + read n) (y + read n * a) a
      go (["up", n] : r) x y a = go r x y (a - read n)
      go (["down", n] : r) x y a = go r x y (a + read n)
      go [] x y a = x * y
   in go (fmap words $ lines input) 0 0 0

run :: IO ()
run = do
  exam <- readFile "input/2021/02.ex.txt"
  input <- readFile "input/2021/02.txt"
  print $ f exam
  print $ f input
