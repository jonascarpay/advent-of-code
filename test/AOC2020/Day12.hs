{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day12 (day12) where

import AOC2020.Common
import Test.Hspec

data Dir = N | E | S | W | F | L | R
  deriving (Show)

go x y wx wy [] = (x, y)
go x y wx wy ((N, n) : t) = go x y wx (wy + n) t
go x y wx wy ((S, n) : t) = go x y wx (wy - n) t
go x y wx wy ((E, n) : t) = go x y (wx + n) wy t
go x y wx wy ((W, n) : t) = go x y (wx - n) wy t
--
go x y wx wy ((L, 90) : t) = go x y (-wy) wx t
go x y wx wy ((L, 180) : t) = go x y (-wx) (-wy) t
go x y wx wy ((L, 270) : t) = go x y wy (-wx) t
go x y wx wy ((R, 90) : t) = go x y wy (-wx) t
go x y wx wy ((R, 180) : t) = go x y (-wx) (-wy) t
go x y wx wy ((R, 270) : t) = go x y (-wy) wx t
--
go x y wx wy ((F, n) : t) = go (x + n * wx) (y + n * wy) wx wy t

parseD :: String -> (Dir, Int)
parseD ('N' : n) = (N, read n)
parseD ('S' : n) = (S, read n)
parseD ('E' : n) = (E, read n)
parseD ('W' : n) = (W, read n)
parseD ('R' : n) = (R, read n)
parseD ('L' : n) = (L, read n)
parseD ('F' : n) = (F, read n)

day12 :: Spec
day12 = do
  input <- runIO $ fmap parseD . lines <$> readFile "input/2020/day12.txt"
  let (x, y) = go 0 0 10 1 input
  star1 2228 $ 0
  star2 42908 $ abs x + abs y

--- 2228
--- 42908
