{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-imports #-}

module AOC2021.Day04 where

import Control.Monad
import Data.Map (Map)
import Data.Map qualified as M
import Lib
import Linear
import Parse
import TestLib

type Bingo = Map (V2 Int) (Int, Bool)

pBoard :: Parser Bingo
pBoard = do
  pSpace
  nrs <- replicateM 5 $ replicateM 5 $ lexeme decimal
  pure $ M.fromList [(V2 x y, (nr, False)) | (row, y) <- zip nrs [0 ..], (nr, x) <- zip row [0 ..]]

parser :: Parser ([Int], [Bingo])
parser = do
  nrs <- sepBy decimal (char ',')
  bingo <- some pBoard
  pure (nrs, bingo)

mark :: Int -> Bingo -> Bingo
mark n = fmap (\(x, b) -> if x == n then (x, True) else (x, b))

hasBingo :: Bingo -> Bool
hasBingo b = any (all (\x -> snd $ b M.! x)) (rows <> cols)
  where
    rows = [flip V2 y <$> [0 .. 4] | y <- [0 .. 4]]
    cols = [V2 x <$> [0 .. 4] | x <- [0 .. 4]]

ex1 :: [Int] -> [Bingo] -> Int
ex1 (nr : nrs) bingos = case filter hasBingo bingos' of
  (h : _) -> nr * sum (fmap fst $ filter (not . snd) $ M.elems h)
  _ -> ex1 nrs bingos'
  where
    bingos' = fmap (mark nr) bingos

ex2 :: [Int] -> [Bingo] -> Int
ex2 (nr : nrs) bingos = case filter (not . hasBingo) bingos' of
  [] -> nr * sum (fmap fst $ filter (not . snd) $ (>>= M.elems) $ mark nr <$> bingos)
  h' -> ex2 nrs h'
  where
    bingos' = fmap (mark nr) bingos

day4 :: Spec
day4 = do
  (nrs', bingos') <- runIO $ parseFile "input/2021/04.ex.txt" parser
  (nrs, bingos) <- runIO $ parseFile "input/2021/04.txt" parser
  star1ex 4512 $ ex1 nrs' bingos'
  star1 71708 $ ex1 nrs bingos
  star2ex 1924 $ ex2 nrs' bingos'
  star2 34726 $ ex2 nrs bingos
