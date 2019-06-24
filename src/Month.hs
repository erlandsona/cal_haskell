module Month where

import qualified Data.Text as T
import Data.List
import Day
import Types

columnWidth, padding :: Int
columnWidth = 20
padding = 3

numOfDays :: Month -> Year -> Day
numOfDays (Month month) (Year year)
  | month == 2 = Day daysInFebruary
  | month `elem` [4, 6, 9, 11] = Day 30
  | otherwise = Day 31
  where daysInFebruary
          | year `mod` 400 == 0 || year `mod` 4 == 0 && year `mod` 100 /= 0 = 29
          | otherwise = 28

months :: [String]
months = ["", "January", "February", "March",
          "April", "May", "June", "July", "August",
          "September", "October", "November", "December"]

header :: Month -> Year -> String
header (Month month) (Year year) = T.unpack .
                    T.center 20 ' ' .
                    T.pack $
                    months !! month ++ " " ++ show year -- "Month Year"

daysString :: String
daysString = "Su Mo Tu We Th Fr Sa"

prefixDayOne :: Month -> Year -> String
prefixDayOne month year =
  let (DayOfWeek first) = (firstDayOfMonth month year (Day 1))
  in
    T.unpack $
    T.justifyRight (padding * first) ' ' $ T.pack ""

pairManipulator :: (String,String) -> (String,String)
pairManipulator (as,bs)
  | all (' '==) as = ("\n", bs)
  | otherwise      = (as ++ "\n", drop 1 bs)

arrOfWeeks :: String -> [String]
arrOfWeeks list
  | list == "" = []
  | otherwise  = fst pair : arrOfWeeks (snd pair)
  where pair = pairManipulator . splitAt 20 $ list

numOfDaysArray :: Month -> Year -> [Day]
numOfDaysArray month year =
  let (Day numDays) = numOfDays month year in
    Day <$> [1..numDays]

properlySpaced :: Month -> Year -> String
properlySpaced month year = prefixDayOne month year ++
          concatMap (\(Day x) -> if x < 9 then ' ':show x ++ " " else ' ':show x) (numOfDaysArray month year)

-- Blackbird combinator
infixr 9 ...
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

grid :: Month -> Year -> String
grid = T.unpack ... T.justifyLeft 124 ' ' ... T.pack ... properlySpaced

chunks :: Month -> Year -> [String]
chunks = arrOfWeeks ... grid

monthNumbers :: Month -> Year -> String
monthNumbers month year
  | any (' '==) . last $ chunkers =
      reverse . (' ':) . drop 1 . reverse . concat $ chunkers
  | otherwise = init . concat $ chunkers
  where chunkers = chunks month year


monthString :: Month -> Year -> String
monthString month year =
  intercalate "\n" [
    header month year,
    daysString,
    monthNumbers month year
  ]
