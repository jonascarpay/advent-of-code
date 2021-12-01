{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020.Day19 (day19) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Text qualified as T
import Parse
import TestLib

data Rule = RC Char | RS [Int] | RB Rule Rule
  deriving (Eq, Show)

parser :: Parser (IntMap Rule, [Text])
parser = do
  rs <- many pRule
  eol
  ts <- many $ takeWhileP Nothing (/= '\n') <* eol
  pure (IM.fromList rs, ts)
  where
    pRule :: Parser (Int, Rule)
    pRule = do
      n <- decimal
      chunk ": "
      r <-
        choice . fmap try $
          [ do
              single '"'
              c <- anySingle
              single '"'
              pure $ RC c,
            do
              l <- many (decimal <* single ' ')
              chunk "| "
              r <- sepBy1 decimal (single ' ')
              pure (RB (RS l) (RS r)),
            do
              l <- sepBy1 decimal (single ' ')
              pure (RS l)
          ]
      eol
      pure $ (n, r)

rmatch :: IntMap Rule -> Text -> Bool
rmatch rs = any null . execStateT (go (rs IM.! 0)) . T.unpack
  where
    go :: Rule -> StateT String [] ()
    go (RC c) = do
      get >>= \case
        (c' : t) | c == c' -> put t
        _ -> lift []
    go (RB l r) = go l <|> go r
    go (RS s) = mapM_ (go . (rs IM.!)) s

day19 :: Spec
day19 = do
  (rs, ts) <- runIO $ parseFile "input/2020/day19.txt" parser
  star1 200 $ length $ filter (rmatch rs) ts
  let rs' =
        IM.insert 8 (RB (RS [42]) (RS [42, 8]))
          . IM.insert 11 (RB (RS [42, 31]) (RS [42, 11, 31]))
          $ rs
  star2 407 $ length $ filter (rmatch rs') ts
