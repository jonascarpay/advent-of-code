{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-imports #-}

module AOC2021.Day03 where

import Data.Histogram qualified as H
import Data.List (partition)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Lib
import TestLib

ex1 :: String -> Int -> Int
ex1 input n =
  let ws = words input
      ix :: [(Int, Char)] = ws >>= zip [0 ..]
      counts = H.fromList ix
      gamma :: Int = fromBE $ do
        i <- take n [0 ..]
        let zs = H.lookup (i, '0') counts
        let os = H.lookup (i, '1') counts
        pure $ os > zs
      epse :: Int = fromBE $ do
        i <- take n [0 ..]
        let zs = H.lookup (i, '0') counts
        let os = H.lookup (i, '1') counts
        pure $ os < zs
   in gamma * epse

ex2 :: String -> Int -> Int
ex2 input n =
  let ws = words input
      vs :: [Vector Bool] = fmap (V.fromList . fmap (== '1')) ws
      go2 :: Int -> [Vector Bool] -> Vector Bool
      go2 _ [a] = a
      go2 i a =
        if i == n
          then head a
          else
            let (os, zs) = partition id $ (V.! i) <$> a
                scr = length os < length zs
             in go2 (i + 1) $ flip filter a $ \vec -> vec V.! i == scr
      co = fromBE $ V.toList $ go2 0 vs
      go :: Int -> [Vector Bool] -> Vector Bool
      go _ [a] = a
      go i a =
        if i == n
          then head a
          else
            let (os, zs) = partition id $ (V.! i) <$> a
                scr = length os >= length zs
             in go (i + 1) $ flip filter a $ \vec -> vec V.! i == scr
      ox = fromBE $ V.toList $ go 0 vs
   in ox * co

day3 :: Spec
day3 = do
  ex <- runIO $ readFile "input/2021/03.ex.txt"
  input <- runIO $ readFile "input/2021/03.txt"
  star1ex 198 $ ex1 ex 5
  star1 3277364 $ ex1 input 12
  star2ex 230 $ ex2 ex 5
  star2 5736383 $ ex2 input 12

-- putStrLn d1
