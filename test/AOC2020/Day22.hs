{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day22 (day22) where

import Data.Set (Set)
import Data.Set qualified as S
import Parse
import TestLib

parse22 :: Parser ([Int], [Int])
parse22 = do
  takeWhileP Nothing (/= '\n') <* eol
  p1 <- many $ decimal <* eol
  eol
  takeWhileP Nothing (/= '\n') <* eol
  p2 <- many $ decimal <* eol
  pure (p1, p2)

combat :: [Int] -> [Int] -> Either [Int] [Int]
combat [] xs = Right xs
combat xs [] = Left xs
combat (l : ls) (r : rs) = case compare l r of
  LT -> combat ls (rs <> [r, l])
  GT -> combat (ls <> [l, r]) rs
  _ -> undefined

playround :: [Int] -> [Int] -> Set ([Int], [Int]) -> Either [Int] [Int]
playround hl hr seen | S.member (hl, hr) seen = Left hl
playround [] hr _ = Right hr
playround hl [] _ = Left hl
playround hl@(l : ls) hr@(r : rs) seen
  | length rs >= r && length ls >= l = case playround (take l ls) (take r rs) mempty of
    Right _ -> playround ls (rs <> [r, l]) seen'
    Left _ -> playround (ls <> [l, r]) rs seen'
  | otherwise = case compare l r of
    LT -> playround ls (rs <> [r, l]) seen'
    GT -> playround (ls <> [l, r]) rs seen'
    _ -> undefined
  where
    seen' = S.insert (hl, hr) seen

ex1 :: [Int] -> [Int] -> Int
ex1 l r = sum $ zipWith (*) (reverse . unEither $ combat l r) [1 ..]

ex2 :: [Int] -> [Int] -> Int
ex2 l r =
  sum . zipWith (*) [1 ..] . reverse . unEither $
    playround l r mempty

unEither :: Either x x -> x
unEither = either id id

day22 :: Spec
day22 = do
  (lEx, rEx) <- runIO $ parseFile "input/2020/day22ex.txt" parse22
  (l, r) <- runIO $ parseFile "input/2020/day22.txt" parse22
  star1ex 306 $ ex1 lEx rEx
  star1 32815 $ ex1 l r
  star2ex 291 $ ex2 lEx rEx
  star2 30695 $ ex2 l r
