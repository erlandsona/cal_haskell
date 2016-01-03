module CalSpec where

import Test.Hspec
import Cal

main :: IO ()
main = hspec spec

spec = parallel $ do
  describe "Cal" $ do
    describe "#zellers" $ do
      it "returns 0" $ do
        zellers 14 1 1800 `shouldBe`  0
      it "returns 1" $ do
        zellers 1 10 2012 `shouldBe`  1
      it "returns 5" $ do
        zellers 14 3 2000 `shouldBe` 2
      it "returns 3" $ do
        zellers 2 6 2021 `shouldBe`  3
      it "returns 4" $ do
        zellers 30 4 2015 `shouldBe`  4
      it "returns 5" $
        zellers 1 1 2000 `shouldBe` 5
      it "returns 6" $ do
        zellers 28 12 2999 `shouldBe`  6
    describe "#dayOfTheWeek" $ do
      it "returns 0" $ do
        dayOfTheWeek 13 2 2000 `shouldBe` 0
      it "returns 1" $ do
        dayOfTheWeek 1 10 2012 `shouldBe`  1
      it "returns 2" $ do
        dayOfTheWeek 14 1 1800 `shouldBe`  2
      it "returns 3" $ do
        dayOfTheWeek 2 6 2021 `shouldBe`  3
      it "returns 4" $ do
        dayOfTheWeek 30 4 2015 `shouldBe`  4
      it "returns 5" $ do
        dayOfTheWeek 27 12 2999 `shouldBe`  5
      it "returns 6" $
        dayOfTheWeek 1 1 2000 `shouldBe` 6
