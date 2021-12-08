module AOC2021.Day08 where

import Control.Monad
import Data.Foldable
import Data.List (permutations)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Lib
import Parse
import Test.Hspec
import TestLib

type Input = [([Set Char], [Set Char])]

parser :: Parser Input
parser = pLines $ do
  ins <- sepEndBy (S.fromList <$> some alphaNumChar) (char ' ')
  skip 2
  outs <- sepBy (S.fromList <$> many alphaNumChar) (char ' ')
  pure (ins, outs)

ex1 :: Input -> Int
ex1 = count (flip elem seg . length) . (>>= snd)
  where
    seg :: [Int]
    seg = [2, 4, 3, 7]

f :: Map (Set Char) Int
f =
  M.fromList
    [ (S.fromList "acedgfb", 8),
      (S.fromList "cdfbe", 5),
      (S.fromList "gcdfa", 2),
      (S.fromList "fbcad", 3),
      (S.fromList "dab", 7),
      (S.fromList "cefabd", 9),
      (S.fromList "cdfgeb", 6),
      (S.fromList "eafb", 4),
      (S.fromList "cagedb", 0),
      (S.fromList "ab", 1)
    ]

dec :: [Set Char] -> Maybe Int
dec = foldM (\acc l -> (acc * 10 +) <$> (f M.!? l)) 0

ex2 :: Input -> Int
ex2 input = sum $ fmap (uncurry decRow) input

decRow :: [Set Char] -> [Set Char] -> Int
decRow lhs rhs = head . (>>= toList) $
  flip fmap (permutations "abcdefg") $ \p ->
    let charmap :: Map Char Char
        charmap = M.fromList (zip ['a' ..] p)
        rhs' :: [Set Char] -> [Set Char]
        rhs' = fmap (S.map (charmap M.!))
     in dec (rhs' lhs) *> dec (rhs' rhs)

day8 :: Spec
day8 = do
  exam <- runIO $ parseFile "input/2021/08.ex.txt" parser
  input <- runIO $ parseFile "input/2021/08.txt" parser
  star1ex 26 $ ex1 exam
  star1 473 $ ex1 input
  star2ex 61229 $ ex2 exam
  star2 1097568 $ ex2 input
