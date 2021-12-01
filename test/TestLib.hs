module TestLib
  ( module TestLib,
    module Test.Hspec,
  )
where

import Test.Hspec

star1, star1ex, star2, star2ex :: (Show a, Eq a) => a -> a -> Spec
star1 expect actual = it "star 1" $ actual `shouldBe` expect
star2 expect actual = it "star 2" $ actual `shouldBe` expect
star1ex expect actual = it "star 1 example" $ actual `shouldBe` expect
star2ex expect actual = it "star 2 example" $ actual `shouldBe` expect
