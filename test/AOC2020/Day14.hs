{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day14 where

import AOC2020.Common
import Control.Monad
import Data.Bits
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Lib
import Parse
import Test.Hspec

data Inst = Mask [Char] | Set Int Int
  deriving (Show)

pp :: Parser [Inst]
pp = many $ (maskbs <|> membs) <* eol
 where
  maskbs = do
    chars <- chunk "mask = " *> replicateM 36 anySingle
    pure $ Mask chars
  membs = do
    chunk "mem["
    k <- decimal
    chunk "] = "
    v <- decimal
    pure $ Set k v

run :: IntMap Int -> Int -> Int -> [Inst] -> IntMap Int
run mem _ _ [] = mem
run mem mand mor (Set k v : t) = run (IM.insert k ((v .&. mand) .|. mor) mem) mand mor t
run mem _ _ (Mask cs : t) = let (ma', mo') = parseMask cs in run mem ma' mo' t

parseMask :: [Char] -> (Int, Int)
parseMask cs = (\(a, b) -> (fromBE a, fromBE b)) $ unzip $ f <$> cs
 where
  f :: Char -> (Bool, Bool)
  f '1' = (True, True)
  f '0' = (False, False)
  f 'X' = (True, False)

run2 :: IntMap Int -> (Int -> [Int]) -> [Inst] -> IntMap Int
run2 mem _ [] = mem
run2 mem f (Set k v : t) = run2 (foldr (\k' m' -> IM.insert k' v m') mem (f k)) f t
run2 mem _ (Mask cs : t) = run2 mem (pmask2 cs) t

pmask2 :: [Char] -> Int -> [Int]
pmask2 m0 b =
  fmap fromBE $
    traverse
      ( \case
          ('1', _) -> [True]
          ('0', n) -> [testBit b n]
          ('X', _) -> [True, False]
      )
      $ zip m0 [35, 34 ..]

day14 :: Spec
day14 = do
  p <- runIO $ parseFile "input/day14.txt" pp
  star1 8566770985168 $ sum . IM.elems $ run mempty 0 0 p
  star2 4832039794082 $ sum . IM.elems $ run2 mempty pure p
