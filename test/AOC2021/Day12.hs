module AOC2021.Day12 (day12) where

import Control.Monad
import Data.Char (isUpper)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Parse
import TestLib

type Input = [(String, String)]

parser :: Parser Input
parser = pLines $ do
  s <- some alphaNumChar
  void $ char '-'
  e <- some alphaNumChar
  pure (s, e)

data Node = Start | End | Big String | Small String | Medium String
  deriving (Eq, Ord, Show)

parseNode :: String -> Node
parseNode "start" = Start
parseNode "end" = End
parseNode str
  | all isUpper str = Big str
  | otherwise = Small str

parseMap :: Input -> Map Node (Set Node)
parseMap = foldr f mempty
  where
    f (a, b) =
      let a' = parseNode a
          b' = parseNode b
       in M.insertWith mappend a' (S.singleton b') . M.insertWith mappend b' (S.singleton a')

ex1 :: Input -> Int
ex1 input = length $ go Start (S.singleton Start) []
  where
    m = parseMap input
    go End _ path = [End : path]
    go current visited path = do
      to <- S.toList $ m M.! current
      guard $ not $ isStart to || (isSmall to && S.member to visited)
      go to (S.insert to visited) (to : path)

ex2 :: Input -> Int
ex2 input = length $ go Start (S.singleton Start) False []
  where
    m = parseMap input
    go End _ _ path = [End : path]
    go current visited doubled path = do
      to <- S.toList $ m M.! current
      guard $ not $ isStart to || (doubled && isSmall to && S.member to visited)
      go to (S.insert to visited) (doubled || (isSmall to && S.member to visited)) (to : path)

isSmall :: Node -> Bool
isSmall (Small _) = True
isSmall _ = False

isStart :: Node -> Bool
isStart Start = True
isStart _ = False

day12 :: Spec
day12 = do
  exam <- runIO $ parseFile "input/2021/12.ex.txt" parser
  input <- runIO $ parseFile "input/2021/12.txt" parser
  star1 226 $ ex1 exam
  star1ex 5958 $ ex1 input
  star2 3509 $ ex2 exam
  star2ex 150426 $ ex2 input
  pure ()
