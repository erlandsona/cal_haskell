module MonthSpec where

import Test.Hspec
import Data.Text (center, pack, unpack)
import Data.List
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
    it "February 2015" $ do
      header 2 2015 `shouldBe` "    February 2015   "
    it "January 2016" $ do
      header 1 2016 `shouldBe` "    January 2016    "

  describe "#daysString" $ do
    it "returns two letter names of days" $
      daysString `shouldBe` "Su Mo Tu We Th Fr Sa"

  describe "#dayOne" $ do
    it "returns prefix for day one" $
      prefixDayOne 1 2017 `shouldBe` ""
    it "returns prefix for day one" $
      prefixDayOne 2 2016 `shouldBe` "   "
    it "returns prefix for day one" $
      prefixDayOne 1 2016 `shouldBe` "               "

  describe "#monthNumbers" $ do

    it "builds Feb 2016" $
      monthNumbers 2 2016 `shouldBe` intercalate "\n" [
        "    1  2  3  4  5  6",
        " 7  8  9 10 11 12 13",
        "14 15 16 17 18 19 20",
        "21 22 23 24 25 26 27",
        "28 29",
        ""
      ]

    it "builds Jan 2012" $
      monthNumbers 1 2012 `shouldBe` intercalate "\n" [
        " 1  2  3  4  5  6  7",
        " 8  9 10 11 12 13 14",
        "15 16 17 18 19 20 21",
        "22 23 24 25 26 27 28",
        "29 30 31",
        ""
      ]

    it "builds Jan 2017" $
      monthNumbers 1 2017 `shouldBe` intercalate "\n" [
        " 1  2  3  4  5  6  7",
        " 8  9 10 11 12 13 14",
        "15 16 17 18 19 20 21",
        "22 23 24 25 26 27 28",
        "29 30 31",
        ""
      ]

    it "builds May 2016" $
      monthNumbers 1 2017 `shouldBe` intercalate "\n" [
        " 1  2  3  4  5  6  7",
        " 8  9 10 11 12 13 14",
        "15 16 17 18 19 20 21",
        "22 23 24 25 26 27 28",
        "29 30 31",
        ""
      ]

    it "builds Jan 2000" $
      monthNumbers 1 2000 `shouldBe` intercalate "\n" [
        "                   1",
        " 2  3  4  5  6  7  8",
        " 9 10 11 12 13 14 15",
        "16 17 18 19 20 21 22",
        "23 24 25 26 27 28 29",
        "30 31"
      ]

    it "builds May 2000" $
      monthNumbers 5 2000 `shouldBe` intercalate "\n" [
        "    1  2  3  4  5  6",
        " 7  8  9 10 11 12 13",
        "14 15 16 17 18 19 20",
        "21 22 23 24 25 26 27",
        "28 29 30 31",
        ""
      ]

    it "builds Feb 2000" $
      monthNumbers 2 2000 `shouldBe` intercalate "\n" [
        "       1  2  3  4  5",
        " 6  7  8  9 10 11 12",
        "13 14 15 16 17 18 19",
        "20 21 22 23 24 25 26",
        "27 28 29",
        ""
      ]

    it "builds March 2000" $
      monthNumbers 3 2000 `shouldBe` intercalate "\n" [
        "          1  2  3  4",
        " 5  6  7  8  9 10 11",
        "12 13 14 15 16 17 18",
        "19 20 21 22 23 24 25",
        "26 27 28 29 30 31",
        ""
      ]

    it "builds June 2000" $
      monthNumbers 6 2000 `shouldBe` intercalate "\n" [
        "             1  2  3",
        " 4  5  6  7  8  9 10",
        "11 12 13 14 15 16 17",
        "18 19 20 21 22 23 24",
        "25 26 27 28 29 30",
        ""
      ]

    it "builds September 2000" $
      monthNumbers 9 2000 `shouldBe` intercalate "\n" [
        "                1  2",
        " 3  4  5  6  7  8  9",
        "10 11 12 13 14 15 16",
        "17 18 19 20 21 22 23",
        "24 25 26 27 28 29 30",
        ""
      ]

    it "builds February 2015" $
      monthNumbers 2 2015 `shouldBe` intercalate "\n" [
        " 1  2  3  4  5  6  7",
        " 8  9 10 11 12 13 14",
        "15 16 17 18 19 20 21",
        "22 23 24 25 26 27 28",
        "",
        ""
      ]







  -- describe "#monthString" $ do
  --   it "builds february 2015" $
  --     monthString 2 2015
  --     `shouldBe`
  --     intercalate "\n" [
  --     "   February 2015    ",
  --     "Su Mo Tu We Th Fr Sa",
  --     " 1  2  3  4  5  6  7",
  --     " 8  9 10 11 12 13 14",
  --     "15 16 17 18 19 20 21",
  --     "22 23 24 25 26 27 28",
  --     "                    ",
  --     "                    "
  --     ]
