module Month.Main where

import qualified Data.Text as T
import Data.List
import Day.Main

type Days = Int
type Week = Int
type Month = Int
type Year = Int

columnWidth, padding :: Int
columnWidth = 20
padding = 3

numOfDays :: Month -> Year -> Days
numOfDays month year
  | month == 2 = daysInFebruary
  | month `elem` [4, 6, 9, 11] = 30
  | otherwise = 31
  where daysInFebruary
          | year `mod` 400 == 0 || year `mod` 4 == 0 && year `mod` 100 /= 0 = 29
          | otherwise = 28

months :: [String]
months = ["", "January", "February", "March",
          "April", "May", "June", "July", "August",
          "September", "October", "November", "December"]

header :: Month -> Year -> String
header month year = T.unpack .
                    T.center 20 ' ' .
                    T.pack $
                    months !! month ++ " " ++ show year -- "Month Year"

daysString :: String
daysString = "Su Mo Tu We Th Fr Sa"

prefixDayOne :: Month -> Year -> String
prefixDayOne month year = T.unpack $
  T.justifyRight (padding * (firstDayOfMonth 1 month year)) ' ' $ T.pack ""

pairManipulator :: (String,String) -> (String,String)
pairManipulator (as,bs)
  | all (' '==) as = ("\n", bs)
  | otherwise      = (as ++ "\n", drop 1 bs)

arrOfWeeks :: String -> [String]
arrOfWeeks list
  | list == "" = []
  | otherwise  = fst pair : arrOfWeeks (snd pair)
  where pair = pairManipulator . splitAt 20 $ list

numOfDaysArray :: Month -> Year -> [Days]
numOfDaysArray month year = [1..numOfDays month year]

properlySpaced :: Month -> Year -> String
properlySpaced month year = prefixDayOne month year ++
          concatMap (\x -> if x < 9 then ' ':show x ++ " " else ' ':show x) (numOfDaysArray month year)

grid :: Month -> Year -> String
grid month year = T.unpack . T.justifyLeft 124 ' ' . T.pack $ properlySpaced month year

chunks :: Month -> Year -> [String]
chunks month year = arrOfWeeks $ grid month year

monthNumbers :: Month -> Year -> String
monthNumbers month year
  | any (' '==) . last $ chunkers =
      reverse . (' ':) . drop 1 . reverse . concat $ chunkers
  | otherwise = init . concat $ chunkers
  where chunkers = chunks month year


monthString :: Int -> Int -> String
monthString month year =
  intercalate "\n" [
    header month year,
    daysString,
    monthNumbers month year
  ]
