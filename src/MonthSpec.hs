module MonthSpec where

import Test.Hspec
import Data.Text (center, pack, unpack)
import Data.List
import Month

main :: IO ()
main = hspec spec

spec = parallel $ describe "Month" $do
  describe "numOfDays" $do
    it "February 2015 has 28 days" $ numOfDays 2 2015 `shouldBe` 28
    it "February 2016 has 29 days" $ numOfDays 2 2016 `shouldBe` 29
    it "April    2016 has 30 days" $ numOfDays 4 2016 `shouldBe` 30
    it "Other months have 31 days" $ numOfDays 5 2016 `shouldBe` 31

  describe "header" $do
    it "February 2015" $do
      header 2 2015 `shouldBe` "    February 2015   "
    it "January 2016" $do
      header 1 2016 `shouldBe` "    January 2016    "

  describe "daysString" $do
    it "returns two letter names of days" $do
      daysString `shouldBe` "Su Mo Tu We Th Fr Sa"

  describe "prefixDayOne" $do
    it "returns prefix for Jan 1st 2017" $do
      prefixDayOne 1 2017 `shouldBe` ""
    it "returns prefix for Feb 1st 2016" $do
      prefixDayOne 2 2016 `shouldBe` "   "
    it "returns prefix for Jan 1st 2016" $do
      prefixDayOne 1 2016 `shouldBe` "               "


  describe "pairManipulator" $do
    describe "takes a pair of strings and returns a pair of strings manipulated." $do
      context "when as are all whitespace" $do
        it "should return a \\n and bs" $do
          pairManipulator ("       ", "      ") `shouldBe`
                          ("\n", "      ")
      context "when as are empty" $do
        it "should return a \\n and bs" $do
          pairManipulator ("", "      ") `shouldBe`
                          ("\n", "      ")
      context "when as got stuff in em" $do
        it "should return a \\n and bs" $do
          pairManipulator ("asdf asdf ", "1234      ") `shouldBe`
                          ("asdf asdf \n", "234      ")

  describe "arrOfWeeks" $do
    context "when list is empty" $do
      it "should return an empty array" $do
        arrOfWeeks "" `shouldBe` []
    context "when list is not empty" $do
      it "should return an array of strings adjusted by the pair manipulator" $do
        arrOfWeeks "012345678901234567890123456789012345678901234567890123456789"
            `shouldBe`
                  ["01234567890123456789\n"
                  ,"12345678901234567890\n"
                  ,"234567890123456789\n"]

  describe "numOfDaysArray" $do
    it "February 2015 has 28 days" $do numOfDaysArray 2 2015 `shouldBe` [1..28]
    it "February 2016 has 29 days" $do numOfDaysArray 2 2016 `shouldBe` [1..29]
    it "April    2016 has 30 days" $do numOfDaysArray 4 2016 `shouldBe` [1..30]
    it "Other months have 31 days" $do numOfDaysArray 5 2016 `shouldBe` [1..31]


  describe "properlySpaced" $do
    describe "separates the numbers with the proper padding according to a 20 column grid." $do
      it "February 2015" $do
        properlySpaced 2 2015 `shouldBe` " 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28"
      it "February 2016" $do
        properlySpaced 2 2016 `shouldBe` "    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29"
      it "April 2016" $do
        properlySpaced 4 2016 `shouldBe` "                1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30"
      it "May 2016" $do
        properlySpaced 5 2016 `shouldBe` " 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31"

  describe "grid" $do
    it "left justifies whatever comes back from properlySpaced." $do
      grid 2 2015 `shouldBe` " 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28                                         "

  describe "chunks" $do
    describe "creates an arrayOfWeeks out of the grid." $do
      it "February 2015" $do
        chunks 2 2015
        `shouldBe`
        [" 1  2  3  4  5  6  7\n"
        ," 8  9 10 11 12 13 14\n"
        ,"15 16 17 18 19 20 21\n"
        ,"22 23 24 25 26 27 28\n"
        ,"\n"
        ,"\n"]

      it "April 2016" $do
        chunks 4 2016
        `shouldBe`
        ["                1  2\n"
        ," 3  4  5  6  7  8  9\n"
        ,"10 11 12 13 14 15 16\n"
        ,"17 18 19 20 21 22 23\n"
        ,"24 25 26 27 28 29 30\n"
        ,"\n"]

      it "May 2016" $do
        chunks 5 2016
        `shouldBe`
        [" 1  2  3  4  5  6  7\n"
        ," 8  9 10 11 12 13 14\n"
        ,"15 16 17 18 19 20 21\n"
        ,"22 23 24 25 26 27 28\n"
        ,"29 30 31            \n"
        ,"\n"]


  describe "monthNumbers" $do

    it "builds Feb 2016" $
      monthNumbers 2 2016 `shouldBe` intercalate "\n" [
         "    1  2  3  4  5  6"
        ," 7  8  9 10 11 12 13"
        ,"14 15 16 17 18 19 20"
        ,"21 22 23 24 25 26 27"
        ,"28 29               "
        ,""]

    it "builds Jan 2012" $
      monthNumbers 1 2012 `shouldBe` intercalate "\n" [
         " 1  2  3  4  5  6  7"
        ," 8  9 10 11 12 13 14"
        ,"15 16 17 18 19 20 21"
        ,"22 23 24 25 26 27 28"
        ,"29 30 31            "
        ,""]

    it "builds Jan 2017" $
      monthNumbers 1 2017 `shouldBe` intercalate "\n" [
         " 1  2  3  4  5  6  7"
        ," 8  9 10 11 12 13 14"
        ,"15 16 17 18 19 20 21"
        ,"22 23 24 25 26 27 28"
        ,"29 30 31            "
        ,""]

    it "builds May 2016" $
      monthNumbers 1 2017 `shouldBe` intercalate "\n" [
         " 1  2  3  4  5  6  7"
        ," 8  9 10 11 12 13 14"
        ,"15 16 17 18 19 20 21"
        ,"22 23 24 25 26 27 28"
        ,"29 30 31            "
        ,""]

    it "builds Jan 2000" $
      monthNumbers 1 2000 `shouldBe` intercalate "\n" [
         "                   1"
        ," 2  3  4  5  6  7  8"
        ," 9 10 11 12 13 14 15"
        ,"16 17 18 19 20 21 22"
        ,"23 24 25 26 27 28 29"
        ,"30 31               "]

    it "builds May 2000" $
      monthNumbers 5 2000 `shouldBe` intercalate "\n" [
         "    1  2  3  4  5  6"
        ," 7  8  9 10 11 12 13"
        ,"14 15 16 17 18 19 20"
        ,"21 22 23 24 25 26 27"
        ,"28 29 30 31         "
        ,""]

    it "builds Feb 2000" $
      monthNumbers 2 2000 `shouldBe` intercalate "\n" [
         "       1  2  3  4  5"
        ," 6  7  8  9 10 11 12"
        ,"13 14 15 16 17 18 19"
        ,"20 21 22 23 24 25 26"
        ,"27 28 29            "
        ,""]

    it "builds March 2000" $
      monthNumbers 3 2000 `shouldBe` intercalate "\n" [
         "          1  2  3  4"
        ," 5  6  7  8  9 10 11"
        ,"12 13 14 15 16 17 18"
        ,"19 20 21 22 23 24 25"
        ,"26 27 28 29 30 31   "
        ,""]

    it "builds June 2000" $
      monthNumbers 6 2000 `shouldBe` intercalate "\n" [
         "             1  2  3"
        ," 4  5  6  7  8  9 10"
        ,"11 12 13 14 15 16 17"
        ,"18 19 20 21 22 23 24"
        ,"25 26 27 28 29 30   "
        ,""]

    it "builds September 2000" $
      monthNumbers 9 2000 `shouldBe` intercalate "\n" [
         "                1  2"
        ," 3  4  5  6  7  8  9"
        ,"10 11 12 13 14 15 16"
        ,"17 18 19 20 21 22 23"
        ,"24 25 26 27 28 29 30"
        ,""]

    it "builds February 2015" $
      monthNumbers 2 2015 `shouldBe` intercalate "\n" [
         " 1  2  3  4  5  6  7"
        ," 8  9 10 11 12 13 14"
        ,"15 16 17 18 19 20 21"
        ,"22 23 24 25 26 27 28"
        ,""
        ,""]

    it "builds January 2016" $
      monthNumbers 1 2016 `shouldBe` intercalate "\n" [
         "                1  2"
        ," 3  4  5  6  7  8  9"
        ,"10 11 12 13 14 15 16"
        ,"17 18 19 20 21 22 23"
        ,"24 25 26 27 28 29 30"
        ,"31                  "]

    it "builds February 2016" $
      monthNumbers 2 2016 `shouldBe` intercalate "\n" [
         "    1  2  3  4  5  6"
        ," 7  8  9 10 11 12 13"
        ,"14 15 16 17 18 19 20"
        ,"21 22 23 24 25 26 27"
        ,"28 29               "
        ,""]


    it "builds March 2016" $
      monthNumbers 3 2016 `shouldBe` intercalate "\n" [
         "       1  2  3  4  5"
        ," 6  7  8  9 10 11 12"
        ,"13 14 15 16 17 18 19"
        ,"20 21 22 23 24 25 26"
        ,"27 28 29 30 31      "
        ,""]

    it "builds April 2016" $
      monthNumbers 4 2016 `shouldBe` intercalate "\n" [
         "                1  2"
        ," 3  4  5  6  7  8  9"
        ,"10 11 12 13 14 15 16"
        ,"17 18 19 20 21 22 23"
        ,"24 25 26 27 28 29 30"
        ,""]

    it "builds May 2016" $
      monthNumbers 5 2016 `shouldBe` intercalate "\n" [
         " 1  2  3  4  5  6  7"
        ," 8  9 10 11 12 13 14"
        ,"15 16 17 18 19 20 21"
        ,"22 23 24 25 26 27 28"
        ,"29 30 31            "
        ,""]

    it "builds June 2016" $
      monthNumbers 6 2016 `shouldBe` intercalate "\n" [
         "          1  2  3  4"
        ," 5  6  7  8  9 10 11"
        ,"12 13 14 15 16 17 18"
        ,"19 20 21 22 23 24 25"
        ,"26 27 28 29 30      "
        ,""]

    it "builds July 2016" $
      monthNumbers 7 2016 `shouldBe` intercalate "\n" [
         "                1  2"
        ," 3  4  5  6  7  8  9"
        ,"10 11 12 13 14 15 16"
        ,"17 18 19 20 21 22 23"
        ,"24 25 26 27 28 29 30"
        ,"31                  "]

    it "builds August 2016" $
      monthNumbers 8 2016 `shouldBe` intercalate "\n" [
         "    1  2  3  4  5  6"
        ," 7  8  9 10 11 12 13"
        ,"14 15 16 17 18 19 20"
        ,"21 22 23 24 25 26 27"
        ,"28 29 30 31         "
        ,""]

    it "builds September 2016" $
      monthNumbers 9 2016 `shouldBe` intercalate "\n" [
         "             1  2  3"
        ," 4  5  6  7  8  9 10"
        ,"11 12 13 14 15 16 17"
        ,"18 19 20 21 22 23 24"
        ,"25 26 27 28 29 30   "
        ,""]

    it "builds October 2016" $
      monthNumbers 10 2016 `shouldBe` intercalate "\n" [
         "                   1"
        ," 2  3  4  5  6  7  8"
        ," 9 10 11 12 13 14 15"
        ,"16 17 18 19 20 21 22"
        ,"23 24 25 26 27 28 29"
        ,"30 31               "]

    it "builds November 2016" $
      monthNumbers 11 2016 `shouldBe` intercalate "\n" [
         "       1  2  3  4  5"
        ," 6  7  8  9 10 11 12"
        ,"13 14 15 16 17 18 19"
        ,"20 21 22 23 24 25 26"
        ,"27 28 29 30         "
        ,""]

    it "builds December 2016" $
      monthNumbers 12 2016 `shouldBe` intercalate "\n" [
         "             1  2  3"
        ," 4  5  6  7  8  9 10"
        ,"11 12 13 14 15 16 17"
        ,"18 19 20 21 22 23 24"
        ,"25 26 27 28 29 30 31"
        ,""]

  describe "monthString" $do
    it "puts the header days and numbers together" $
      monthString 2 2016 `shouldBe` intercalate "\n" [
         "    February 2016   "
        ,"Su Mo Tu We Th Fr Sa"
        ,"    1  2  3  4  5  6"
        ," 7  8  9 10 11 12 13"
        ,"14 15 16 17 18 19 20"
        ,"21 22 23 24 25 26 27"
        ,"28 29               "
        ,""]
