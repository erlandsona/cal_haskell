module DaySpec where

import Test.Hspec
import Day

main :: IO ()
main = hspec spec

spec = parallel $ describe "Day" $ do

  describe "#zellers" $ do

    context "Happy Path" $ do

      it "returns 0" $ zellers 14 1  1800 `shouldBe` 0
      it "returns 1" $ zellers 1  10 2012 `shouldBe` 1
      it "returns 2" $ zellers 14 3  2000 `shouldBe` 2
      it "returns 3" $ zellers 2  6  2021 `shouldBe` 3
      it "returns 4" $ zellers 30 4  2015 `shouldBe` 4
      it "returns 5" $ zellers 1  1  2000 `shouldBe` 5
      it "returns 6" $ zellers 28 12 2999 `shouldBe` 6

    context "Sad Path" $ do

      it "Gives a more meaningful error message" $
        pendingWith "Figure out error handling"

  describe "#dayOfTheWeek" $ do

    context "Happy Path" $ do

      it "returns 0" $ dayOfTheWeek 13 2  2000 `shouldBe` 0
      it "returns 1" $ dayOfTheWeek 1  10 2012 `shouldBe` 1
      it "returns 2" $ dayOfTheWeek 14 1  1800 `shouldBe` 2
      it "returns 3" $ dayOfTheWeek 2  6  2021 `shouldBe` 3
      it "returns 4" $ dayOfTheWeek 30 4  2015 `shouldBe` 4
      it "returns 5" $ dayOfTheWeek 27 12 2999 `shouldBe` 5
      it "returns 6" $ dayOfTheWeek 1  1  2000 `shouldBe` 6

    context "Sad Path" $ do

      it "Gives a more meaningful error message" $
        pendingWith "Figure out error handling"
