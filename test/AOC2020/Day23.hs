{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day23 (day23) where

import Control.Monad
import Control.Monad.ST
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as VM
import TestLib

exinput :: [Int]
exinput = [3, 8, 9, 1, 2, 5, 4, 6, 7]

input :: [Int]
input = [1, 8, 6, 5, 2, 4, 9, 7, 3]

exinput2 :: [Int]
exinput2 = exinput <> [10 .. 1_000_000]

input2 :: [Int]
input2 = input <> [10 .. 1_000_000]

play :: [Int] -> Int -> Vector Int
play elems n = V.map (+ 1) fromNexts
  where
    nexts' = V.create $ do
      arr <- V.thaw nexts
      iterateM_ (playround arr) (head elems - 1) n
      pure arr
    fromNexts = V.unfoldrN len go 0
      where
        go i =
          let next = nexts' V.! i
           in if next == 0 then Nothing else Just (next, next)
    nexts = V.create $ do
      v <- VM.replicate len (-1)
      let pairs =
            let from0 = fmap (subtract 1) elems
                elems' = from0 <> [head from0]
             in zip elems' (tail elems')
      forM_ pairs (uncurry $ VM.write v)
      pure v
    len = length elems

playround :: STVector s Int -> Int -> ST s Int
playround nexts esrc = do
  a <- VM.read nexts esrc
  b <- VM.read nexts a
  c <- VM.read nexts b
  let edst = go (esrc - 1)
        where
          go n
            | n < 0 = go (VM.length nexts - 1)
            | n == a || n == b || n == c = go (n - 1)
            | otherwise = n
  VM.read nexts c >>= VM.write nexts esrc
  VM.read nexts edst >>= VM.write nexts c
  VM.write nexts edst a
  VM.read nexts esrc

iterateM_ :: Monad m => (a -> m a) -> a -> Int -> m ()
iterateM_ f = go
  where
    go _ n | n < 1 = pure ()
    go a n = f a >>= \a' -> go a' (n - 1)

day23 :: Spec
day23 = do
  star1ex (V.fromList [5, 4, 6, 7, 3, 2, 8, 9]) $ play exinput 1
  star1ex (V.fromList [3, 2, 5, 4, 6, 7, 8, 9]) $ play exinput 2
  star1ex (V.fromList [3, 4, 6, 7, 2, 5, 8, 9]) $ play exinput 3

  star1ex (V.fromList [9, 2, 6, 5, 8, 3, 7, 4]) $ play exinput 10
  star1ex (V.fromList [6, 7, 3, 8, 4, 5, 2, 9]) $ play exinput 100

  star1 (V.fromList [4, 5, 9, 8, 3, 6, 2, 7]) $ play input 100

  star2ex 149245887792 $ V.product $ V.take 2 $ play exinput2 10_000_000
  star2 111080192688 $ V.product $ V.take 2 $ play input2 10_000_000 -- too low
