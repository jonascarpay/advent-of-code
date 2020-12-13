{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-imports #-}

module Runners where

import Block
import Control.Monad
import Data.Char
import Data.Foldable (toList)
import Data.List
import Data.List.Split
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Sequence qualified as Q
import Data.Set (Set)
import Data.Set qualified as S
import Data.Vector qualified as V
import Debug.Trace
import Lib
import Linear hiding (E)
import Parse
import Text.Megaparsec
