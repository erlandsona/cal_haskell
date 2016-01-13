module Month where

import Data.Ratio
import qualified Data.Text as T
import Data.List
import Day

columnWidth = 20
padding = 3

daysInFebruary year
  | year `mod` 400 == 0 || year `mod` 4 == 0 && year `mod` 100 /= 0 = 29
  | otherwise = 28

numOfDays month year
  | month == 2 = daysInFebruary year
  | month `elem` [4, 6, 9, 11] = 30
  | otherwise = 31


months = [undefined, "January", "February", "March",
          "April", "May", "June", "July", "August", 
          "September", "October", "November", "December"]


-- header :: Int -> Int -> T.Text
header month year = T.unpack .
                    T.center 20 ' ' .
                    T.pack $
                    months !! month ++ " " ++ show year -- "Month Year"

daysString :: String
daysString = "Su Mo Tu We Th Fr Sa"

prefixDayOne month year = T.unpack $
  T.justifyRight (padding * (firstDayOfMonth 1 month year)) ' ' $ T.pack ""

replaceEvery :: Int -> Char -> String -> String
replaceEvery nth replacement (head:tail)
  | !! `mod` nth == 0 = replacement:tail
  | otherwise = x:replaceEvery (count+1) replacement tail 

monthNumbers month year = 
  grid
  where grid           = properlySpaced
        properlySpaced = prefixDayOne month year ++
          concatMap adjuster [1..numOfDays month year]
        adjuster x
          | x < 9 = ' ':show x ++ " "
          | otherwise = ' ':show x
