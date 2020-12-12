{-# LANGUAGE OverloadedStrings #-}

module AOC2020.Day07 (day7) where

import AOC2020.Common
import Control.Applicative hiding (many)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Parse
import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer qualified as Lex

day7 :: Spec
day7 = do
  contains <- runIO $ parseFile "input/day7.txt" parseBags
  let sg = Bag "shiny" "gold"
  star1 274 $
    let m :: Map Bag (Set Bag) -- containedby
        m = M.fromListWith (<>) [(c', S.singleton c) | (c, cs) <- contains, (_, c') <- cs]
        go :: Set Bag -> Set Bag
        go set =
          let set' = set <> mconcat (fromMaybe mempty . flip M.lookup m <$> S.toList set)
           in if set == set' then set else go set'
     in length $ go (S.singleton sg)
  star2 158730 $
    let m :: Map Bag [(Int, Bag)] -- contains
        m = M.fromList contains
        go :: Bag -> Int
        go b = maybe 0 (sum . fmap (\(n, b') -> n + n * go b')) $ M.lookup b m
     in go sg

data Bag = Bag ByteString ByteString
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
    let nDec = Lex.decimal <* chunk " "
    bs <- ([] <$ chunk "no other bags") <|> (sepBy (liftA2 (,) nDec pBag) (chunk ", "))
    chunk "."
    pure $ (bHead, bs)
