{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day21 (day21) where

import Data.Foldable
import Data.Histogram qualified as H
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Lib
import Parse
import TestLib

pp :: Parser [([Text], [Text])]
pp = do
  let sub = do
        ws <- some (word <* single ' ')
        chunk "(contains "
        is <- sepBy word (chunk ", ")
        chunk ")"
        pure $ (ws, is)
  many $ sub <* eol

solve2 :: [([Text], [Text])] -> (Int, Text)
solve2 cands = (occurs, dangers)
  where
    occurs :: Int
    occurs = sum $ (`H.lookup` nosh) <$> nos
    dangers :: Text
    dangers = T.intercalate "," $ fmap snd $ fromJust $ assignUnique (fmap S.toList <$> M.toList allergs)
    nosh :: H.Histogram Text
    nosh = H.fromList (cands >>= fst)
    nos :: [Text]
    nos = filter (\f -> S.notMember f hasAllerg) foods
    allergs :: Map Text (Set Text)
    allergs = go mempty cands
    foods :: [Text]
    foods = S.toList $ S.fromList $ cands >>= fst
    hasAllerg :: Set Text
    hasAllerg = fold $ M.elems $ allergs
    go :: Map Text (Set Text) -> [([Text], [Text])] -> Map Text (Set Text)
    go m [] = m
    go m ((is, as) : t) =
      let m' = foldr (\a n -> M.insertWith S.intersection a (S.fromList is) n) m as
       in go m' t

day21 :: Spec
day21 = do
  p <- runIO $ parseFile "input/2020/day21.txt" pp
  let (s1, s2) = solve2 p
  star1 2324 s1
  star2 "bxjvzk,hqgqj,sp,spl,hsksz,qzzzf,fmpgn,tpnnkc" s2
