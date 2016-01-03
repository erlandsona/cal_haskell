{-# LANGUAGE QuasiQuotes #-}
module Month where

import Data.Ratio
import Text.RawString.QQ
import Data.Text

daysInFebruary year
  | year `mod` 400 == 0 || year `mod` 4 == 0 && year `mod` 100 /= 0 = 29
  | otherwise = 28

thirtyDayMonth month = month `elem` [4, 6, 9, 11]

numOfDays month year
  | month == 2 = daysInFebruary year
  | thirtyDayMonth month = 30
  | otherwise = 31


columnWidth = 20
padding = 3
months = [undefined, "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]


-- header :: Int -> Int -> Text
header month year = unpack $
  center 20 ' ' (pack $ months !! month ++ " " ++ show year)
daysString = "Su Mo Tu We Th Fr Sa"
-- monthNumbers = 
--   days_of_month = String.new.rjust(first_day_of_month * PADDING)
--   num_of_weeks = 1

--   1.upto(num_of_days) do |i|
--     date = i.to_s
--     obj = {true => date.center(PADDING), false => date.ljust(PADDING)}
--     if days_of_month.length > COLUMN_WIDTH
--       str << days_of_month.rstrip + "\n"
--       days_of_month = ""
--       num_of_weeks += 1
--     end
--     days_of_month << obj[i <= 9]
--   end

--   num_of_newlines = String.new
--   while num_of_weeks <= 6
--     num_of_newlines += "\n"
--     num_of_weeks += 1
--   end
--   str << days_of_month.rstrip + num_of_newlines
-- end





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

