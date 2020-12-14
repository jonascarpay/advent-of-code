{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day11 (day11) where

import AOC2020.Common
import Block
import Data.Foldable (toList)
import Lib
import Parse
import Test.Hspec

data Square = Free | Occ | Floor
  deriving (Eq, Show)

step1 :: Block Square -> Block Square
step1 sq = imap f sq
 where
  kings (x, y) = [(x', y') | x' <- [x -1, x, x + 1], y' <- [y -1, y, y + 1], not (x' == x && y == y')]
  f :: (Int, Int) -> Square -> Square
  f c q =
    let surr = (>>= toList) $ (\(x, y) -> bIndex x y sq) <$> kings c :: [Square]
     in case (q, length (filter (== Occ) surr)) of
          (Free, n) | n == 0 -> Occ
          (Occ, n) | n >= 4 -> Free
          (q', _) -> q'

dir :: [(Int, Int)]
dir = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], not (x == 0 && y == 0)]

step2 :: Block Square -> Block Square
step2 sq = imap f sq
 where
  f :: (Int, Int) -> Square -> Square
  f (x, y) q = case (q, length (filter id surr)) of
    (Free, n) | n == 0 -> Occ
    (Occ, n) | n >= 5 -> Free
    (q', _) -> q'
   where
    surr = scan (x, y) <$> dir
    scan (x, y) (dx, dy) = go (x + dx, y + dy)
     where
      go (x, y) = case bIndex x y sq of
        Just Occ -> True
        Nothing -> False
        Just Free -> False
        _ -> go (x + dx, y + dy)

day11 :: Spec
day11 = do
  let f 'L' = Free
      f '.' = Floor
      f '#' = Occ
  seats <- runIO $ fmap f <$> parseFile "input/day11.txt" pBlock
  star1 2368 $ length $ filter (== Occ) $ bList $ findFix step1 seats
  star2 2124 $ length $ filter (== Occ) $ bList $ findFix step2 seats
