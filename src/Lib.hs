{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

{-- fluff
uniquePairs :: [a] -> [(a, a)]
uniquePairs [] = []
uniquePairs (a : t) = (fmap (a,) t) <> uniquePairs t

uniqueTriplets :: [a] -> [(a, a, a)]
uniqueTriplets [] = []
uniqueTriplets (a : t) = (tcons a <$> uniquePairs t) <> uniqueTriplets t
 where
  tcons a (b, c) = (a, b, c)
--}

uniques :: Traversable t => t () -> [a] -> [t a]
uniques base as =
  flip evalStateT as $
    forM base $
      const $
        fix $ \f ->
          get >>= \case
            [] -> empty
            (h : t) -> put t >> (pure h <|> f)

uniqueTuples :: (Traversable t, Applicative t) => [a] -> [t a]
uniqueTuples = uniques (pure ())

readListOfInts :: FilePath -> IO [Int]
readListOfInts = fmap parseInts . readFile
 where
  parseInts :: String -> [Int]
  parseInts = fmap read . lines
