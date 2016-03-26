module Month where

import Data.Ratio
import qualified Data.Text as T
import Data.List
import Day

columnWidth = 20
padding = 3

numOfDays :: Int -> Int -> Int
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

header :: Int -> Int -> String
header month year = T.unpack .
                    T.center 20 ' ' .
                    T.pack $
                    months !! month ++ " " ++ show year -- "Month Year"

daysString :: String
daysString = "Su Mo Tu We Th Fr Sa"

prefixDayOne :: Int -> Int -> String
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

monthNumbers :: Int -> Int -> String
monthNumbers month year
  | any (' '==) (last chunks) =
      reverse . (' ':) . drop 1 . reverse . concat $ chunks
  | otherwise = init . concat $ chunks
  where numOfDaysArray = [1..numOfDays month year]
        properlySpaced = prefixDayOne month year ++
          concatMap (\x -> if x < 9 then ' ':show x ++ " " else ' ':show x) numOfDaysArray
        grid           = T.unpack . T.justifyLeft 124 ' ' $ T.pack properlySpaced
        chunks = arrOfWeeks grid


monthString :: Int -> Int -> String
monthString month year =
  intercalate "\n" [
    header month year,
    daysString,
    monthNumbers month year
  ]
