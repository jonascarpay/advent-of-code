{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Runners where

import Block
import Control.Monad
import Data.List.Split
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Vector qualified as V
import Lib
import Linear
import Parse
import Text.Megaparsec
import Text.Megaparsec.Byte.Lexer qualified as Lex

day1 :: IO ()
day1 = do
  input :: [Int] <- parseFile "input/day1.txt" $ pLines Lex.decimal
  print
    . fmap product
    . filter ((== 2020) . sum)
    . uniqueTuples @V2
    $ input
  print
    . fmap product
    . filter ((== 2020) . sum)
    . uniqueTuples @V3
    $ input

day2 :: IO ()
day2 = do
  input <- parseFile "input/day2.txt" $ pLines parsePw
  print $ length $ filter validPass input
  print $ length $ filter validPass2 input

day3 :: IO ()
day3 = do
  block <- parseFile "input/day3.txt" pBlock
  print $ tobbogan 3 1 block
  print $
    product
      [ tobbogan 1 1 block
      , tobbogan 3 1 block
      , tobbogan 5 1 block
      , tobbogan 7 1 block
      , tobbogan 1 2 block
      ]

day4 :: IO ()
day4 = do
  pps <- parseFile "input/day4.txt" (parsePP <* eof)
  print $ length $ filter (\pp -> all (uncurry validField) pp && validpp pp) pps

day5 :: IO ()
day5 = do
  ls <- lines <$> readFile "input/day5.txt"
  print $ maximum $ fmap parseL ls
  print $ missing ls

day6 :: IO ()
day6 = do
  gs <- filter (not . null) . splitOn [""] . lines <$> readFile "input/day6.txt"
  print $ length $ [() | g <- gs, c <- ['a' .. 'z'], all (elem c) g]
  print $ length $ [() | g <- gs, c <- ['a' .. 'z'], any (elem c) g]

day7 :: IO ()
day7 = do
  contains <- parseFile "input/day7.txt" parseBags
  let sg = Bag "shiny" "gold"
  print $
    let m :: Map Bag (Set Bag) -- containedby
        m = M.fromListWith (<>) [(c', S.singleton c) | (c, cs) <- contains, (_, c') <- cs]
        go :: Set Bag -> Set Bag
        go set =
          let set' = set <> mconcat (fromMaybe mempty . flip M.lookup m <$> S.toList set)
           in if set == set' then set else go set'
     in length $ go (S.singleton sg)
  print $
    let m :: Map Bag [(Int, Bag)] -- contains
        m = M.fromList contains
        go :: Bag -> Int
        go b = maybe 0 (sum . fmap (\(n, b') -> n + n * go b')) $ M.lookup b m
     in go sg

data Op
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving (Show)

parseop :: [String] -> Op
parseop [op, '+' : n] = parseop [op, n]
parseop ["acc", n] = Acc (read n)
parseop ["jmp", n] = Jmp (read n)
parseop ["nop", n] = Nop (read n)

is :: Op -> Bool
is (Acc _) = False
is _ = True

toggle (Jmp n) = Nop n
toggle (Nop n) = Jmp n

day8 :: IO ()
day8 = do
  prg <- V.fromList . fmap (parseop . words) . filter (/= []) . lines <$> readFile "input/day8.txt"
  let go acc pc pcs
        | S.member pc pcs = acc
        | otherwise = case prg V.! pc of
          Jmp n -> let pc' = pc + n in go acc pc' (S.insert pc pcs)
          Acc n -> let pc' = pc + 1 in go (acc + n) pc' (S.insert pc pcs)
          Nop _ -> let pc' = pc + 1 in go acc pc' (S.insert pc pcs)
  let go2 v acc pc pcs
        | S.member pc pcs = Nothing
        | pc >= V.length v = Just acc
        | otherwise = case v V.! pc of
          Jmp n -> let pc' = pc + n in go2 v acc pc' (S.insert pc pcs)
          Acc n -> let pc' = pc + 1 in go2 v (acc + n) pc' (S.insert pc pcs)
          Nop _ -> let pc' = pc + 1 in go2 v acc pc' (S.insert pc pcs)

  print $ go 0 0 mempty
  print $
    V.filter isJust $
      fmap
        (\n -> go2 (prg V.// [(n, toggle (prg V.! n))]) 0 0 mempty)
        (V.findIndices is prg)
