{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns -Wno-unused-imports #-}

module Runners where

import Block
import Control.Applicative hiding (many, some)
import Control.Lens
import Control.Monad
import Control.Monad.Combinators qualified as P
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
import Data.List (nub, sort)
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
import Data.Text.IO qualified as TIO
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace
import Lib
import Linear hiding (E, rotate, trace)
import Parse hiding (State)

type Input = ()

parser :: Parser Input
parser = pure ()

ex1 :: Input -> Int
ex1 input = 0

ex2 :: Input -> Int
ex2 input = 0

run :: IO ()
run = do
  exam <- parseFile "input/2021/TODO.ex.txt" parser
  print $ ex1 exam
  -- input <- parseFile "input/2021/TODO.txt" parser
  -- print $ ex1 input
  -- print $ ex2 exam
  -- print $ ex2 input
  pure ()
