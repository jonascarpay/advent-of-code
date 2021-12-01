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
import Lib
import Linear hiding (E, rotate, trace)
import Parse hiding (State)

ex1 :: String -> Int
ex1 = length . filter (ordered (<)) . slide2 . fmap (read @Int) . lines

ex2 :: String -> Int
ex2 = length . filter (ordered (<)) . slide2 . fmap sum . slide3 . fmap (read @Int) . lines

run :: IO ()
run = do
  ex <- readFile "input/2021/01.ex.txt"
  -- d1 <- readFile "input/2021/01.txt"
  print $ ex2 ex

-- putStrLn d1
