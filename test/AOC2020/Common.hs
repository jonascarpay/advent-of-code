module AOC2020.Common where

import Test.Hspec

star1, star2 :: (Show a, Eq a) => a -> a -> Spec
star1 expect actual = it "star 1" $ actual `shouldBe` expect
star2 expect actual = it "star 2" $ actual `shouldBe` expect
