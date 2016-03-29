module YearSpec where

import Test.Hspec
import Data.Text (center, pack, unpack)
import Data.List
import Year

main :: IO ()
main = hspec spec

spec = parallel $ describe "Year" $do

  describe "titleString" $do
    it "pads the year by 64 char" $do
      titleString 2016 `shouldBe` intercalate "\n" [
        "                              2016                              "]

  describe "spacerString" $do
    it "prints a blank 64 char string after the year." $do
      spacerString `shouldBe`
        "                                                                "

  describe "monthNamePaddedArray" $do
    it "returns array of 20 space padded months" $do
      monthNamePaddedArr `shouldBe`
        ["       January      "
        ,"      February      "
        ,"        March       "
        ,"        April       "
        ,"         May        "
        ,"        June        "
        ,"        July        "
        ,"       August       "
        ,"      September     "
        ,"       October      "
        ,"      November      "
        ,"      December      "]

  describe "threeMonthHeader" $do
    describe "outputs the string with the appropriate row of month names" $do
      it "Row1" $do threeMonthHeader 1
        `shouldBe`
        "       January              February                March       "
      it "Row2" $do threeMonthHeader 2
        `shouldBe`
        "        April                  May                  June        "
      it "Row3" $do threeMonthHeader 3
        `shouldBe`
        "        July                 August               September     "
      it "Row4" $do threeMonthHeader 4
        `shouldBe`
        "       October              November              December      "

  describe "yearDaysOfWeek" $do
    it "should print three weeks worth of days" $do
      yearDaysOfWeek
      `shouldBe`
      "Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa"


  describe "pack20" $do
    describe "it is a composition of Data.text functions to align a string on 20 columns." $do
      it "outputs a 20 column leftJustified string" $do
        pack20 "asdf" `shouldBe` "asdf                "

  describe "threeMonthsWeekNumbers" $do
    describe "should return a string of three months of numbers" $do
      it "Week1 Row1 2016" $do
        threeMonthsWeekNumbers 1 1 2016
        `shouldBe`
        "                1  2      1  2  3  4  5  6         1  2  3  4  5"
      it "Week2 Row1 2016" $do
        threeMonthsWeekNumbers 2 1 2016
        `shouldBe`
        " 3  4  5  6  7  8  9   7  8  9 10 11 12 13   6  7  8  9 10 11 12"
      it "Week3 Row1 2016" $do
        threeMonthsWeekNumbers 3 1 2016
        `shouldBe`
        "10 11 12 13 14 15 16  14 15 16 17 18 19 20  13 14 15 16 17 18 19"
      it "Week4 Row1 2016" $do
        threeMonthsWeekNumbers 4 1 2016
        `shouldBe`
        "17 18 19 20 21 22 23  21 22 23 24 25 26 27  20 21 22 23 24 25 26"
      it "Week5 Row1 2016" $do
        threeMonthsWeekNumbers 5 1 2016
        `shouldBe`
        "24 25 26 27 28 29 30  28 29                 27 28 29 30 31      "
      it "Week6 Row1 2016" $do
        threeMonthsWeekNumbers 6 1 2016
        `shouldBe`
        "31                                                              "
      it "Week1 Row2 2016" $do
        threeMonthsWeekNumbers 1 2 2016
        `shouldBe`
        "                1  2   1  2  3  4  5  6  7            1  2  3  4"

  describe "yearString" $do
    it "builds 2016" $do
      yearString 2016 `shouldBe` intercalate "\n" [
        "                              2016                              ",
        "                                                                ",
        "       January              February                March       ",
        "Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa",
        "                1  2      1  2  3  4  5  6         1  2  3  4  5",
        " 3  4  5  6  7  8  9   7  8  9 10 11 12 13   6  7  8  9 10 11 12",
        "10 11 12 13 14 15 16  14 15 16 17 18 19 20  13 14 15 16 17 18 19",
        "17 18 19 20 21 22 23  21 22 23 24 25 26 27  20 21 22 23 24 25 26",
        "24 25 26 27 28 29 30  28 29                 27 28 29 30 31      ",
        "31                                                              ",
        "        April                  May                  June        ",
        "Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa",
        "                1  2   1  2  3  4  5  6  7            1  2  3  4",
        " 3  4  5  6  7  8  9   8  9 10 11 12 13 14   5  6  7  8  9 10 11",
        "10 11 12 13 14 15 16  15 16 17 18 19 20 21  12 13 14 15 16 17 18",
        "17 18 19 20 21 22 23  22 23 24 25 26 27 28  19 20 21 22 23 24 25",
        "24 25 26 27 28 29 30  29 30 31              26 27 28 29 30      ",
        "                                                                ",
        "        July                 August               September     ",
        "Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa",
        "                1  2      1  2  3  4  5  6               1  2  3",
        " 3  4  5  6  7  8  9   7  8  9 10 11 12 13   4  5  6  7  8  9 10",
        "10 11 12 13 14 15 16  14 15 16 17 18 19 20  11 12 13 14 15 16 17",
        "17 18 19 20 21 22 23  21 22 23 24 25 26 27  18 19 20 21 22 23 24",
        "24 25 26 27 28 29 30  28 29 30 31           25 26 27 28 29 30   ",
        "31                                                              ",
        "       October              November              December      ",
        "Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa",
        "                   1         1  2  3  4  5               1  2  3",
        " 2  3  4  5  6  7  8   6  7  8  9 10 11 12   4  5  6  7  8  9 10",
        " 9 10 11 12 13 14 15  13 14 15 16 17 18 19  11 12 13 14 15 16 17",
        "16 17 18 19 20 21 22  20 21 22 23 24 25 26  18 19 20 21 22 23 24",
        "23 24 25 26 27 28 29  27 28 29 30           25 26 27 28 29 30 31",
        "30 31                                                           "]

