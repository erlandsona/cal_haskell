module YearSpec where

import Test.Hspec
import Data.Text (center, pack, unpack)
import Data.List
import Year

main :: IO ()
main = hspec spec

spec = parallel $ describe "Year" $ do
  describe "titleString" $ do
    it "pads the year by 66 char" $
      titleString 2016 `shouldBe` intercalate "\n" [
        "                              2016                              "
      ]

  describe "spacerString" $ do
    it "prints a blank 66 char string after the year." $
      spacerString `shouldBe`
        "                                                                "

  describe "monthNamePaddedArray" $ do
    it "returns array of 20 space padded months" $
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

  describe "twelveMonths" $ do
    it "should return the 12 months with short headers zipped together" $
      twelveMonths 2016 `shouldBe` intercalate "\n"
        ["      January               February               March        "
        ,"Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa"
        ,"                1  2      1  2  3  4  5  6         1  2  3  4  5"
        ," 3  4  5  6  7  8  9   7  8  9 10 11 12 13   6  7  8  9 10 11 12"
        ,"10 11 12 13 14 15 16  14 15 16 17 18 19 20  13 14 15 16 17 18 19"
        ,"17 18 19 20 21 22 23  21 22 23 24 25 26 27  20 21 22 23 24 25 26"
        ,"24 25 26 27 28 29 30  28 29                 27 28 29 30 31      "
        ,"31                                                              "
        ,"       April                  May                   June        "
        ,"Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa"
        ,"                1  2   1  2  3  4  5  6  7            1  2  3  4"
        ," 3  4  5  6  7  8  9   8  9 10 11 12 13 14   5  6  7  8  9 10 11"
        ,"10 11 12 13 14 15 16  15 16 17 18 19 20 21  12 13 14 15 16 17 18"
        ,"17 18 19 20 21 22 23  22 23 24 25 26 27 28  19 20 21 22 23 24 25"
        ,"24 25 26 27 28 29 30  29 30 31              26 27 28 29 30      "
        ,"                                                                "
        ,"        July                 August              September      "
        ,"Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa"
        ,"                1  2      1  2  3  4  5  6               1  2  3"
        ," 3  4  5  6  7  8  9   7  8  9 10 11 12 13   4  5  6  7  8  9 10"
        ,"10 11 12 13 14 15 16  14 15 16 17 18 19 20  11 12 13 14 15 16 17"
        ,"17 18 19 20 21 22 23  21 22 23 24 25 26 27  18 19 20 21 22 23 24"
        ,"24 25 26 27 28 29 30  28 29 30 31           25 26 27 28 29 30   "
        ,"31                                                              "
        ,"      October               November              December      "
        ,"Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa"
        ,"                   1         1  2  3  4  5               1  2  3"
        ," 2  3  4  5  6  7  8   6  7  8  9 10 11 12   4  5  6  7  8  9 10"
        ," 9 10 11 12 13 14 15  13 14 15 16 17 18 19  11 12 13 14 15 16 17"
        ,"16 17 18 19 20 21 22  20 21 22 23 24 25 26  18 19 20 21 22 23 24"
        ,"23 24 25 26 27 28 29  27 28 29 30           25 26 27 28 29 30 31"
        ,"30 31                                                           "]

--     it "builds Feb 2016" $
--       yearString 2016 `shouldBe` intercalate "\n" [
--         "                              2016                              ",
--         "                                                                ",
--         "      January               February               March        ",
--         "Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa",
--         "                1  2      1  2  3  4  5  6         1  2  3  4  5",
--         " 3  4  5  6  7  8  9   7  8  9 10 11 12 13   6  7  8  9 10 11 12",
--         "10 11 12 13 14 15 16  14 15 16 17 18 19 20  13 14 15 16 17 18 19",
--         "17 18 19 20 21 22 23  21 22 23 24 25 26 27  20 21 22 23 24 25 26",
--         "24 25 26 27 28 29 30  28 29                 27 28 29 30 31      ",
--         "31                                                              ",
--         "       April                  May                   June        ",
--         "Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa",
--         "                1  2   1  2  3  4  5  6  7            1  2  3  4",
--         " 3  4  5  6  7  8  9   8  9 10 11 12 13 14   5  6  7  8  9 10 11",
--         "10 11 12 13 14 15 16  15 16 17 18 19 20 21  12 13 14 15 16 17 18",
--         "17 18 19 20 21 22 23  22 23 24 25 26 27 28  19 20 21 22 23 24 25",
--         "24 25 26 27 28 29 30  29 30 31              26 27 28 29 30      ",
--         "                                                                ",
--         "        July                 August              September      ",
--         "Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa",
--         "                1  2      1  2  3  4  5  6               1  2  3",
--         " 3  4  5  6  7  8  9   7  8  9 10 11 12 13   4  5  6  7  8  9 10",
--         "10 11 12 13 14 15 16  14 15 16 17 18 19 20  11 12 13 14 15 16 17",
--         "17 18 19 20 21 22 23  21 22 23 24 25 26 27  18 19 20 21 22 23 24",
--         "24 25 26 27 28 29 30  28 29 30 31           25 26 27 28 29 30   ",
--         "31                                                              ",
--         "      October               November              December      ",
--         "Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa",
--         "                   1         1  2  3  4  5               1  2  3",
--         " 2  3  4  5  6  7  8   6  7  8  9 10 11 12   4  5  6  7  8  9 10",
--         " 9 10 11 12 13 14 15  13 14 15 16 17 18 19  11 12 13 14 15 16 17",
--         "16 17 18 19 20 21 22  20 21 22 23 24 25 26  18 19 20 21 22 23 24",
--         "23 24 25 26 27 28 29  27 28 29 30           25 26 27 28 29 30 31",
--         "30 31                                                           "
--       ]

