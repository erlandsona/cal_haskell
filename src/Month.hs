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

monthNumbers month year = 
  intercalate "\n" . map (reverse . drop 1 . reverse . T.unpack) $ T.chunksOf 20 grid
  where numOfDaysArray = [1..numOfDays month year]
        properlySpaced = prefixDayOne month year ++
          concatMap (\x -> if x < 10 then ' ':show x ++ " " else ' ':show x) numOfDaysArray
        grid           = T.justifyLeft 120 ' ' $ T.pack properlySpaced

-- monthString month year = 
-- [r|line1
--    line2
--    line3|]
--   unlines [
--     "   February 2015    ",
--     "Su Mo Tu We Th Fr Sa",
--     " 1  2  3  4  5  6  7",
--     " 8  9 10 11 12 13 14",
--     "15 16 17 18 19 20 21",
--     "22 23 24 25 26 27 28",
--     "                    ",
--     "                    "
--   ]

