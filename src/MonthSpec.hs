{-# LANGUAGE QuasiQuotes #-}
module MonthSpec where

import Test.Hspec
import Text.RawString.QQ
import Data.Text
import Month

main :: IO ()
main = hspec spec

spec = parallel $ describe "Month" $ do
  describe "#daysInFebruary" $ do
    it "2015 returns 28" $ daysInFebruary 2015 `shouldBe` 28
    it "2016 returns 29" $ daysInFebruary 2016 `shouldBe` 29
  describe "#thirtyDayMonth" $ do
    it "April returns True"  $ thirtyDayMonth 4 `shouldBe` True
    it "May   returns False" $ thirtyDayMonth 5 `shouldBe` False
  describe "#numOfDays" $ do
    it "February 2015 has 28 days" $ numOfDays 2 2015 `shouldBe` 28
    it "February 2016 has 29 days" $ numOfDays 2 2016 `shouldBe` 29
    it "April    2016 has 30 days" $ numOfDays 4 2016 `shouldBe` 30
    it "Other months have 31 days" $ numOfDays 5 2016 `shouldBe` 31
  describe "#header" $ do
    it "builds 20 columns with name of Month and Year" $ do
      header 2 2015 `shouldBe` "    February 2015   "
  describe "#daysString" $ do
    it "returns two letter names of days" $
      daysString `shouldBe` "Su Mo Tu We Th Fr Sa"
  describe "#monthNumbers" $ do
    it "builds a string formatted with days of the week." $
      pending
  -- describe "#monthString" $ do
  --   it "builds february 2015" $
  --     monthString 2 2015
  --     `shouldBe`
  --     unlines [
  --     "   February 2015    ",
  --     "Su Mo Tu We Th Fr Sa",
  --     " 1  2  3  4  5  6  7",
  --     " 8  9 10 11 12 13 14",
  --     "15 16 17 18 19 20 21",
  --     "22 23 24 25 26 27 28",
  --     "                    ",
  --     "                    "
  --     ]
