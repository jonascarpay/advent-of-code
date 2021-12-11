{-# LANGUAGE TupleSections #-}

module AOC2021.Day11 where

import Data.Char (ord)
import Data.Map (Map)
import Data.Map qualified as M
import Lib
import Linear
import Parse
import TestLib

type Input = Map (V2 Int) Int

parser :: Parser Input
parser = fmap toMap $ pLines $ many (toInt <$> alphaNumChar)
  where
    toInt :: Char -> Int
    toInt = subtract (ord '0') . ord
    toMap :: [[Int]] -> Map (V2 Int) Int
    toMap = M.fromList . coords

ex1 :: Input -> Int
ex1 input = go input 100 0
  where
    go :: Map (V2 Int) Int -> Int -> Int -> Int
    go _ 0 f = f
    go x n f =
      let (x', f') = flash (fmap (+ 1) x)
       in go x' (n - 1) (f + f')

ex2 :: Input -> Int
ex2 input = go input 0
  where
    go x n
      | ordered (==) x = n
      | otherwise =
        let (x', _) = flash (fmap (+ 1) x)
         in go x' (n + 1)

flash :: Map (V2 Int) Int -> (Map (V2 Int) Int, Int)
flash x = go ((,True) <$> x) 0
  where
    go x n =
      case filter (\(_, (l, f)) -> l > 9 && f) (M.toList x) of
        [] -> (fmap (\(d, f) -> if f then d else 0) x, n)
        vs ->
          let x' = foldr (M.adjust (\(d, f) -> (d + 1, f))) x (vs >>= adjacent . fst)
           in go (M.fromList ((\(v, _) -> (v, (0, False))) <$> vs) <> x') (n + length vs)

day11 :: Spec
day11 = do
  exam <- runIO $ parseFile "input/2021/11.ex.txt" parser
  input <- runIO $ parseFile "input/2021/11.txt" parser
  star1ex 1656 $ ex1 exam
  star1 1725 $ ex1 input
  star2ex 195 $ ex2 exam
  star2 308 $ ex2 input
