{-# LANGUAGE OverloadedStrings #-}

module AOC2020.Day07 (day7) where

import AOC2020.Common
import Control.Applicative hiding (many)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Parse

s1 :: [(Bag, [(Int, Bag)])] -> Int
s1 contains =
  let m :: Map Bag (Set Bag) -- containedby
      m = M.fromListWith (<>) [(c', S.singleton c) | (c, cs) <- contains, (_, c') <- cs]
      go :: Set Bag -> Set Bag
      go set =
        let set' = set <> mconcat (fromMaybe mempty . flip M.lookup m <$> S.toList set)
         in if set == set' then set else go set'
   in length (go (S.singleton sg)) -1

s2 :: [(Bag, [(Int, Bag)])] -> Int
s2 contains =
  let m :: Map Bag [(Int, Bag)] -- contains
      m = M.fromList contains
      go :: Bag -> Int
      go b = maybe 0 (sum . fmap (\(n, b') -> n + n * go b')) $ M.lookup b m
   in go sg

day7 :: Spec
day7 = do
  containsEx1 <- runIO $ parseFile "input/day7ex.txt" parseBags
  containsEx2 <- runIO $ parseFile "input/day7ex2.txt" parseBags
  contains <- runIO $ parseFile "input/day7.txt" parseBags
  star1ex 4 $ s1 containsEx1
  star1 274 $ s1 contains
  star2ex 126 $ s2 containsEx2
  star2 158730 $ s2 contains

sg :: Bag
sg = Bag "shiny" "gold"

data Bag = Bag Text Text
  deriving (Eq, Ord, Show)

parseBags :: Parser [(Bag, [(Int, Bag)])]
parseBags = many (pContains <* eol) <* takeRest
  where
    pBag :: Parser Bag
    pBag = do
      w1 <- word <* chunk " "
      w2 <- word <* chunk " "
      chunk "bags" <|> chunk "bag"
      pure (Bag w1 w2)
    pContains :: Parser (Bag, [(Int, Bag)])
    pContains = do
      bHead <- pBag <* chunk " contain "
      let nDec = decimal <* chunk " "
      bs <- ([] <$ chunk "no other bags") <|> (sepBy (liftA2 (,) nDec pBag) (chunk ", "))
      chunk "."
      pure (bHead, bs)
