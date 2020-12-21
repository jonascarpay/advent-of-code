{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day11 (day11) where

import AOC2020.Common
import Block
import Data.Foldable (toList)
import Lib
import Linear
import Parse

data Square = Free | Occ | Floor
  deriving (Eq, Show)

step1 :: Block Square -> Block Square
step1 sq = imap f sq
 where
  f :: V2 Int -> Square -> Square
  f c q =
    let surr = (>>= toList) $ (\x -> bIndex x sq) <$> adjacent c :: [Square]
     in case (q, length (filter (== Occ) surr)) of
          (Free, n) | n == 0 -> Occ
          (Occ, n) | n >= 4 -> Free
          (q', _) -> q'

step2 :: Block Square -> Block Square
step2 sq = imap f sq
 where
  f :: V2 Int -> Square -> Square
  f v q = case (q, length (filter id surr)) of
    (Free, n) | n == 0 -> Occ
    (Occ, n) | n >= 5 -> Free
    (q', _) -> q'
   where
    surr = scan v <$> adjacent 0
    scan x d = go (x + d)
     where
      go w = case bIndex w sq of
        Just Occ -> True
        Nothing -> False
        Just Free -> False
        _ -> go (w + d)

day11 :: Spec
day11 = do
  let f 'L' = Free
      f '.' = Floor
      f '#' = Occ
  seats <- runIO $ fmap f <$> parseFile "input/day11.txt" pBlock
  star1 2368 $ length $ filter (== Occ) $ bList $ findFix step1 seats
  star2 2124 $ length $ filter (== Occ) $ bList $ findFix step2 seats
