{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day24 (day24) where

import AOC2020.Common
import Data.Set (Set)
import Data.Set qualified as S
import Histogram qualified as H
import Linear hiding (E)
import Parse

data Dir = E | SE | SW | W | NW | NE
  deriving (Show, Enum)

parse24 :: Parser [[Dir]]
parse24 = some pdir `sepEndBy` eol
  where
    pdir =
      choice
        [ E <$ chunk "e",
          W <$ chunk "w",
          SE <$ chunk "se",
          SW <$ chunk "sw",
          NE <$ chunk "ne",
          NW <$ chunk "nw"
        ]

toC :: Dir -> V3 Int
toC NE = V3 1 (-1) 0
toC E = V3 1 0 (-1)
toC SE = V3 0 1 (-1)
toC SW = V3 (-1) 1 0
toC W = V3 (-1) 0 1
toC NW = V3 0 (-1) 1

hexadj :: V3 Int -> [V3 Int]
hexadj v = (v +) . toC <$> [E .. NE]

step :: Set (V3 Int) -> Set (V3 Int)
step s = S.fromList $ filter next adjacents
  where
    adjacents :: [V3 Int]
    adjacents = S.toList s >>= hexadj
    next :: V3 Int -> Bool
    next v =
      let n = length $ filter (`S.member` s) (hexadj v)
          t = S.member v s
       in if t
            then not $ n == 0 || n > 2
            else n == 2

s1 :: [[Dir]] -> Int
s1 = length . filter (odd . snd) . H.count . fmap (sum . fmap toC)

s2 :: [[Dir]] -> [Int]
s2 = fmap S.size . iterate step . S.fromList . fmap fst . filter (odd . snd) . H.count . fmap (sum . fmap toC)

day24 :: Spec
day24 = do
  p <- runIO $ parseFile "input/day24.txt" parse24
  pex <- runIO $ parseFile "input/day24ex.txt" parse24
  star1ex 10 $ s1 pex
  star1 420 $ s1 p
  star2ex 2208 $ s2 pex !! 100
  star2 4206 $ s2 p !! 100
