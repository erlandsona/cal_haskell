module DaySpec where

import Test.Hspec
  ( Spec
  , context
  , describe
  , hspec
  , it
  , parallel
  , pendingWith
  , shouldBe
  )

import Day

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ describe "Day" $ do
  describe "zellers" $ do

    context "Happy Path" $ do

      it "returns 0" $ zellers 14 1  1800 `shouldBe` 0
      it "returns 1" $ zellers 1  10 2012 `shouldBe` 1
      it "returns 2" $ zellers 14 3  2000 `shouldBe` 2
      it "returns 3" $ zellers 2  6  2021 `shouldBe` 3
      it "returns 4" $ zellers 30 4  2015 `shouldBe` 4
      it "returns 5" $ zellers 1  1  2000 `shouldBe` 5
      it "returns 6" $ zellers 28 12 2999 `shouldBe` 6

    context "Sad Path" $

      it "Gives a more meaningful error message" $
        pendingWith "Figure out error handling"

  describe "firstDayOfMonth" $ do

    context "Happy Path" $ do

      it "returns 0" $ firstDayOfMonth 13 2  2000 `shouldBe` 0
      it "returns 1" $ firstDayOfMonth 1  10 2012 `shouldBe` 1
      it "returns 2" $ firstDayOfMonth 14 1  1800 `shouldBe` 2
      it "returns 3" $ firstDayOfMonth 2  6  2021 `shouldBe` 3
      it "returns 4" $ firstDayOfMonth 30 4  2015 `shouldBe` 4
      it "returns 5" $ firstDayOfMonth 27 12 2999 `shouldBe` 5
      it "returns 6" $ firstDayOfMonth 1  1  2000 `shouldBe` 6

    context "Sad Path" $

      it "Gives a more meaningful error message" $
        pendingWith "Figure out error handling"
