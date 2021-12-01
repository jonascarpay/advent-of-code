{-# LANGUAGE OverloadedStrings #-}

module AOC2020.Day08 (day8) where

import AOC2020.Common
import Data.Maybe
import Data.Set qualified as S
import Data.Vector qualified as V
import Test.Hspec

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

toggle :: Op -> Op
toggle (Jmp n) = Nop n
toggle (Nop n) = Jmp n

day8 :: Spec
day8 = do
  prg <- runIO $ V.fromList . fmap (parseop . words) . filter (/= []) . lines <$> readFile "input/2020/day8.txt"
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

  star1 1521 $ go 0 0 mempty
  star2 1016
    . fromJust
    . V.head
    . V.filter isJust
    . fmap (\n -> go2 (prg V.// [(n, toggle (prg V.! n))]) 0 0 mempty)
    . V.findIndices is
    $ prg
