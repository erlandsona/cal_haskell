{-# LANGUAGE QuasiQuotes #-}
module Month where

import Text.RawString.QQ
import Data.Ratio

daysInFebruary year
  | year `mod` 400 == 0 || year `mod` 4 == 0 && year `mod` 100 /= 0 = 29
  | otherwise = 28

thirtyDayMonth month = month `elem` [4, 6, 9, 11]

numOfDays month year
  | month == 2 = daysInFebruary year
  | thirtyDayMonth month = 30
  | otherwise = 31

monthString month year = 
  [r|
   February 2015    
Su Mo Tu We Th Fr Sa
  |]
  -- unlines [
  -- "   February 2015    "
  -- "Su Mo Tu We Th Fr Sa",
  -- " 1  2  3  4  5  6  7",
  -- " 8  9 10 11 12 13 14",
  -- "15 16 17 18 19 20 21",
  -- "22 23 24 25 26 27 28",
  -- "                    ",
  -- "                    "
  -- ]

