{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AOC2020.Day12 (day12) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Parse hiding (State)
import Test.Hspec
import TestLib

type Input = [(Char, Int)]

data Sub = Sub
  { _x :: Int,
    _y :: Int,
    _wx :: Int,
    _wy :: Int,
    _dir :: Int
  }

sub0 :: Sub
sub0 = Sub 0 0 10 (-1) 0

makeLenses ''Sub

parser :: Parser Input
parser = pLines $ liftA2 (,) anySingle decimal

ex1 :: Input -> Int
ex1 input = ans $
  flip execState sub0 $
    forM input $ \case
      ('N', n) -> y -= n
      ('S', n) -> y += n
      ('E', n) -> x += n
      ('W', n) -> x -= n
      ('L', 90) -> dir -= 1
      ('L', 180) -> dir -= 2
      ('L', 270) -> dir -= 3
      ('R', 90) -> dir += 1
      ('R', 180) -> dir += 2
      ('R', 270) -> dir += 3
      ('F', n) -> do
        d <- use dir
        case mod d 4 of
          0 -> x += n
          1 -> y += n
          2 -> x -= n
          3 -> y -= n

ans :: Sub -> Int
ans s = abs (s ^. x) + abs (s ^. y)

ex2 :: Input -> Int
ex2 input = ans $
  flip execState sub0 $
    forM input $ \case
      ('N', n) -> wy -= n
      ('S', n) -> wy += n
      ('E', n) -> wx += n
      ('W', n) -> wx -= n
      ('L', 90) -> rotL
      ('L', 180) -> replicateM_ 2 rotL
      ('L', 270) -> replicateM_ 3 rotL
      ('R', 90) -> replicateM_ 3 rotL
      ('R', 180) -> replicateM_ 2 rotL
      ('R', 270) -> rotL
      ('F', n) -> do
        wx' <- use wx
        wy' <- use wy
        x += n * wx'
        y += n * wy'
  where
    rotL = do
      wx' <- use wx
      wy' <- use wy
      wx .= wy'
      wy .= (-wx')

day12 :: Spec
day12 = do
  exam <- runIO $ parseFile "input/2020/day12ex.txt" parser
  input <- runIO $ parseFile "input/2020/day12.txt" parser
  star1ex 25 $ ex1 exam
  star1 2228 $ ex1 input
  star2ex 286 $ ex2 exam
  star2 42908 $ ex2 input
