{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day22 (day22) where

import AOC2020.Common
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace
import Histogram qualified as H
import Lib
import Parse

parse22 :: Parser ([Int], [Int])
parse22 = do
  takeWhileP Nothing (/= '\n') <* eol
  p1 <- many $ decimal <* eol
  eol
  takeWhileP Nothing (/= '\n') <* eol
  p2 <- many $ decimal <* eol
  pure (p1, p2)

play [] xs = Right xs
play xs [] = Left xs
play (l : ls) (r : rs) = case compare l r of
  LT -> play ls (rs <> [r, l])
  GT -> play (ls <> [l, r]) rs
  _ -> undefined

type Hand = Seq Int

type Memo = Map (Hand, Hand) (Either Hand Hand)

insertAll :: Ord k => [k] -> v -> Map k v -> Map k v
insertAll ks v m = foldr (\k n -> M.insert k v n) m ks

playround :: Set (Hand, Hand) -> [(Hand, Hand)] -> Memo -> Hand -> Hand -> (Memo, Either Hand Hand)
playround c1 hs s h1 h2
  | Seq.null h1 = (insertAll ((h1, h2) : hs) (Right h2) s, Right h2)
  | Seq.null h2 = (insertAll ((h1, h2) : hs) (Left h1) s, Left h1)
  | S.member (h1, h2) c1 = (insertAll ((h1, h2) : hs) (Right h1) s, Right h1)
  | M.member (h1, h2) s = (s, s M.! (h1, h2))
playround c1 hs s h1@(l :<| ls) h2@(r :<| rs)
  | length ls >= l && length rs >= r = case playround mempty [] s ls rs of
    (s', Right res) -> playround (S.insert (h1, h2) c1) [] (insertAll ((h1, h2) : hs) (Right res) s') ls (rs :|> r :|> l)
    (s', Left res) -> playround (S.insert (h1, h2) c1) [] (insertAll ((h1, h2) : hs) (Left res) s') (ls :|> l :|> r) rs
playround c1 hs s h1@(l :<| ls) h2@(r :<| rs) =
  case compare l r of
    LT -> playround (S.insert (h1, h2) c1) ((h1, h2) : hs) s ls (rs :|> r :|> l)
    GT -> playround (S.insert (h1, h2) c1) ((h1, h2) : hs) s (ls :|> l :|> r) rs
    _ -> undefined

ex1 :: [Int] -> [Int] -> Int
ex1 l r = sum $ zipWith (*) (reverse . f $ play l r) [1 ..]

ex2 :: [Int] -> [Int] -> Int
ex2 l r =
  sum . zipWith (*) [1 ..] . reverse . toList . f . snd $
    playround mempty mempty mempty (Seq.fromList l) (Seq.fromList r)

f :: Either x x -> x
f = either id id

day22 :: Spec
day22 = do
  (lEx, rEx) <- runIO $ parseFile "input/day22ex.txt" parse22
  (l, r) <- runIO $ parseFile "input/day22.txt" parse22
  -- input <- lines <$> readFile "input/day22.txt"
  star1ex 306 $ ex1 lEx rEx
  star1 32815 $ ex1 l r

  star2ex 291 $ ex2 lEx rEx

-- star2 291 $ sum $ zipWith (*) (reverse . toList . f $ x) [1 ..]
