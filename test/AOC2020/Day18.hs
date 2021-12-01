{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC2020.Day18 (day18) where

import Control.Monad
import Data.List.Split (splitOn)
import Parse
import TestLib

data Tkn = Num Int | Add | Mul | Paren [Tkn]
  deriving (Eq, Show)

pexpr :: Parser [Tkn]
pexpr =
  many
    ( choice
        [ Num <$> decimal,
          Add <$ chunk " + ",
          Mul <$ void (chunk " * "),
          Paren <$> (chunk "(" *> pexpr <* chunk ")")
        ]
    )

eval :: [Tkn] -> Int
eval = go . reverse
  where
    go (Num n : Add : t) = n + go t
    go (Paren ns : Add : t) = eval ns + go t
    go (Num n : Mul : t) = n * go t
    go (Paren ns : Mul : t) = eval ns * go t
    go [Num n] = n
    go [Paren ns] = eval ns

eval2 :: [Tkn] -> Int
eval2 ts = product $ go <$> ts'
  where
    ts' :: [[Tkn]]
    ts' = splitOn [Mul] ts
    go :: [Tkn] -> Int
    go ts =
      sum $
        ts >>= \case
          Num n -> [n]
          Paren ns -> [eval2 ns]
          Add -> []

day18 :: Spec
day18 = do
  p <- runIO $ parseFile "input/2020/day18.txt" (many (pexpr <* eol))
  star1 9535936849815 $ sum $ eval <$> p
  star1 472171581333710 $ sum $ eval2 <$> p
