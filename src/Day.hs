module Day where

import Data.Ratio

-----------------------------------
--
-- The following method implements Zeller's Congruence an algorithm to determin what day of the week it is.
--
--
-- · h is the day of the week (0 = Sunday, 1 = Monday, 2 = Tuesday, ..., 6 = Saturday)
--
-- Check out the wikipedia article for more info...
-- http://en.wikipedia.org/wiki/Zeller's_congruence
--
---------------------------

zellers :: Int -> Int -> Int -> Int
zellers day month year =
       (day
  +     floor ((month + 1) * 26 % 10)
  +     year
  +     floor (year % 4)
  +     6
  *     floor (year % 100)
  +     floor (year % 400)
  `mod` 7
  +     6)
  `mod` 7

-- firstDayOfMonth :: Integral a => a -> a -> a -> a
firstDayOfMonth :: Int -> Int -> Int -> Int
firstDayOfMonth day month year
  | month <= 2 = zellers (month + 12) day (year - 1)
  | otherwise = zellers day month year

-- Ruby implementaion...
-- def zellers
--   @month += 12 and @year -= 1 if @month <= 2
--   h = (@day + ((26 * (@month + 1)) / 10).floor + @year + (@year / 4).floor + (6 * (@year/100).floor) + (@year/400).floor) % 7
--   ((h + 6) % 7)
-- end
