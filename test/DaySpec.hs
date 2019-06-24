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
import Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ describe "Day" $ do
  describe "zellers" $ do

    context "Happy Path" $ do

      it "returns 0" $ zellers 1 (Year 1800) (Day 14) `shouldBe` (DayOfWeek 0)
      it "returns 1" $ zellers 10 (Year 2012) (Day 1 ) `shouldBe` (DayOfWeek 1)
      it "returns 2" $ zellers 3 (Year 2000) (Day 14) `shouldBe` (DayOfWeek 2)
      it "returns 3" $ zellers 6 (Year 2021) (Day 2 ) `shouldBe` (DayOfWeek 3)
      it "returns 4" $ zellers 4 (Year 2015) (Day 30) `shouldBe` (DayOfWeek 4)
      it "returns 5" $ zellers 1 (Year 2000) (Day 1 ) `shouldBe` (DayOfWeek 5)
      it "returns 6" $ zellers 12 (Year 2999) (Day 28) `shouldBe` (DayOfWeek 6)

    context "Sad Path" $

      it "Gives a more meaningful error message" $
        pendingWith "Figure out error handling"

  describe "firstDayOfMonth" $ do

    context "Happy Path" $ do

      it "returns 0" $ firstDayOfMonth February (Year 2000) (Day 13) `shouldBe` (DayOfWeek 0)
      it "returns 1" $ firstDayOfMonth October (Year 2012) (Day 1 ) `shouldBe` (DayOfWeek 1)
      it "returns 2" $ firstDayOfMonth January (Year 1800) (Day 14) `shouldBe` (DayOfWeek 2)
      it "returns 3" $ firstDayOfMonth June (Year 2021) (Day 2 ) `shouldBe` (DayOfWeek 3)
      it "returns 4" $ firstDayOfMonth April (Year 2015) (Day 30) `shouldBe` (DayOfWeek 4)
      it "returns 5" $ firstDayOfMonth December (Year 2999) (Day 27) `shouldBe` (DayOfWeek 5)
      it "returns 6" $ firstDayOfMonth January (Year 2000) (Day 1 ) `shouldBe` (DayOfWeek 6)

    context "Sad Path" $

      it "Gives a more meaningful error message" $
        pendingWith "Figure out error handling"
