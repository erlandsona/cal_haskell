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


-- replaceEvery :: Int -> Char -> String
-- replaceEvery k = go
--   where
--     go t = case splitAt k t of
--              (a,b) | null a    -> []
--                    | otherwise -> a : go b


-- replaceEvery :: Int -> Char -> String -> String
-- replaceEvery int char string =
  


monthNumbers month year = 
  grid
  where grid           = T.chunksOf 20 $ T.pack properlySpaced
        properlySpaced = prefixDayOne month year ++
          concatMap adjuster [1..numOfDays month year]
        adjuster x
          | x < 9 = ' ':show x ++ " "
          | otherwise = ' ':show x

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

