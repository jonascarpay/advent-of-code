import AOC2020
import AOC2021
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "AOC 2020" aoc2020
  describe "AOC 2021" aoc2021
