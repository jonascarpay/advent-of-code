{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Block
import Control.Applicative hiding (many)
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntSet qualified as IS
import Data.Word
import Debug.Trace
import Parse
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer qualified as Lex

findFix :: Eq a => (a -> a) -> (a -> a)
findFix f = go where go a = let a' = f a in if a == a' then a else go a'

uniqueTuples :: (Traversable t, Applicative t) => [a] -> [t a]
uniqueTuples = uniques (pure ())
 where
  uniques :: Traversable t => t () -> [a] -> [t a]
  uniques base as =
    flip evalStateT as $
      forM base $
        const $
          fix $ \f ->
            get >>= \case
              [] -> empty
              (h : t) -> put t >> (pure h <|> f)
