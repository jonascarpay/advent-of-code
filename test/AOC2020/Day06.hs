{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day06 (day6) where

import AOC2020.Common
import Data.List.Split hiding (chunk, sepBy)
import Lib
import Linear
import Parse
import Test.Hspec
import Text.Megaparsec.Byte.Lexer qualified as Lex

day6 :: Spec
day6 = do
  gs <- runIO $ filter (not . null) . splitOn [""] . lines <$> readFile "input/day6.txt"
  star1 3117 $ length [() | g <- gs, c <- ['a' .. 'z'], all (elem c) g]
  star2 6680 $ length [() | g <- gs, c <- ['a' .. 'z'], any (elem c) g]
