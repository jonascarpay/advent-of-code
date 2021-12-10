{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AOC2021.Day10 (day10) where

import Data.Foldable
import Data.List (sort)
import Parse
import TestLib

type Input = [String]

parser :: Parser Input
parser = pLines $ many $ oneOf "(){}[]<>"

ex1 :: Input -> Int
ex1 input = sum $ fmap (go []) input
  where
    go _ [] = 0
    go s ('(' : t) = go ('(' : s) t
    go s ('{' : t) = go ('{' : s) t
    go s ('[' : t) = go ('[' : s) t
    go s ('<' : t) = go ('<' : s) t
    go ('<' : s) ('>' : t) = go s t
    go ('{' : s) ('}' : t) = go s t
    go ('[' : s) (']' : t) = go s t
    go ('(' : s) (')' : t) = go s t
    go _ ('>' : _) = 25137
    go _ ('}' : _) = 1197
    go _ (']' : _) = 57
    go _ (')' : _) = 3

ex2 :: Input -> Int
ex2 input = middle $ sort (input >>= toList . go [] :: [Int])
  where
    score :: Char -> Int -> Int
    score '(' s = s * 5 + 1
    score '{' s = s * 5 + 3
    score '[' s = s * 5 + 2
    score '<' s = s * 5 + 4

    middle x = x !! div (length x) 2

    go :: String -> String -> Maybe Int
    go s [] = Just $ foldl' (flip score) 0 s
    go s ('(' : t) = go ('(' : s) t
    go s ('{' : t) = go ('{' : s) t
    go s ('[' : t) = go ('[' : s) t
    go s ('<' : t) = go ('<' : s) t
    go ('<' : s) ('>' : t) = go s t
    go ('{' : s) ('}' : t) = go s t
    go ('[' : s) (']' : t) = go s t
    go ('(' : s) (')' : t) = go s t
    go _ ('>' : _) = Nothing
    go _ ('}' : _) = Nothing
    go _ (']' : _) = Nothing
    go _ (')' : _) = Nothing

day10 :: Spec
day10 = do
  exam <- runIO $ parseFile "input/2021/10.ex.txt" parser
  input <- runIO $ parseFile "input/2021/10.txt" parser
  star1ex 26397 $ ex1 exam
  star1 240123 $ ex1 input
  star2ex 288957 $ ex2 exam
  star2 3260812321 $ ex2 input
