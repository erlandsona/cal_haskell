module Year.Main where

import Data.List (intercalate)
import Data.Text (justifyLeft, center, pack, unpack)
import Data.String.Utils (strip, rstrip)

import Month.Main (Year, Week, daysString, chunks, months)

type MonthRow = Int

yearPadding, yearNewLine :: [String] -> String
yearPadding = intercalate "  "
yearNewLine = intercalate "\n"

titleString :: Year -> String
titleString year = unpack . center 64 ' ' $ pack . strip . show $ year

spacerString, yearDaysOfWeek :: String
spacerString = "                                                                "
yearDaysOfWeek = yearPadding $ replicate 3 daysString

monthNamePaddedArr :: [String]
monthNamePaddedArr = map (unpack . center 20 ' ' . pack) $ drop 1 months

threeMonthHeader :: MonthRow -> String
threeMonthHeader monthRow = yearPadding $ take 3 $ drop ((monthRow - 1) * 3) monthNamePaddedArr

pack20 :: String -> String
pack20 = unpack . justifyLeft 20 ' ' . pack . rstrip

threeMonthsWeekNumbers :: Week -> MonthRow -> Year -> String
threeMonthsWeekNumbers week monthRow year =
  yearPadding [packedChunks ((monthRow * 3) - 2)
              ,packedChunks ((monthRow * 3) - 1)
              ,packedChunks (monthRow * 3)]
  where packedChunks m = pack20 $ chunks m year !! (week - 1)

threeMonths :: MonthRow -> Year -> String
threeMonths monthRow year =
  yearNewLine
    [threeMonthHeader monthRow
    ,yearDaysOfWeek
    ,threeMonthsWeekNumbers 1 monthRow year
    ,threeMonthsWeekNumbers 2 monthRow year
    ,threeMonthsWeekNumbers 3 monthRow year
    ,threeMonthsWeekNumbers 4 monthRow year
    ,threeMonthsWeekNumbers 5 monthRow year
    ,threeMonthsWeekNumbers 6 monthRow year]

yearString :: Year -> String
yearString year =
  yearNewLine
    [titleString year
    ,spacerString
    ,threeMonths 1 year
    ,threeMonths 2 year
    ,threeMonths 3 year
    ,threeMonths 4 year
    ]
