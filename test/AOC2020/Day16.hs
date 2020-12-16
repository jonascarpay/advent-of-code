{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module AOC2020.Day16 (day16) where

import AOC2020.Common
import Control.Monad
import Data.Maybe
import Data.Text qualified as T
import Lib
import Parse
import Test.Hspec

data Rule = Rule Text Int Int Int Int
  deriving (Show)

type Ticket = [Int]

prule :: Parser Rule
prule = do
  s <- takeWhileP Nothing (/= ':')
  chunk ": "
  al <- decimal
  chunk "-"
  ar <- decimal
  chunk " or "
  bl <- decimal
  chunk "-"
  br <- decimal
  pure $ Rule s al ar bl br

parser :: Parser ([Rule], Ticket, [Ticket])
parser = do
  rules <- many (try prule <* eol)
  eol
  chunk "your ticket:" <* eol
  self <- sepBy decimal (chunk ",") <* eol
  eol
  chunk "nearby tickets:" <* eol
  others <- many $ sepBy decimal (chunk ",") <* eol
  pure (rules, self, others)

matchf :: Rule -> Int -> Bool
matchf (Rule _ a b x y) f = (f >= a && f <= b) || (f >= x && f <= y)

day16 :: Spec
day16 = do
  (rs, self, others) <- runIO $ parseFile "input/day16.txt" parser
  star1 20091 $ sum [f | t <- others, f <- t, not $ any (`matchf` f) rs]
  star2 2325343130651 $
    let nf = length self
        valid = flip filter others $ \t -> all (\f -> any (`matchf` f) rs) t
        cand r = (r,) $ do
          c <- take nf [0 ..]
          guard $ all (matchf r) ((!! c) <$> valid)
          pure c
        rSolv = fromJust $ assignUnique $ cand <$> rs
     in product $
          rSolv >>= \(Rule str _ _ _ _, n) -> do
            guard $ "departure" `T.isPrefixOf` str
            pure $ self !! n
