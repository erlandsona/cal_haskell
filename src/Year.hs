module Year where

import Data.List
import Data.Text (justifyLeft, center, pack, unpack)
import Data.String.Utils (strip, rstrip)
import Month

type MonthRow = Int

yearPadding :: [String] -> String
yearPadding = intercalate "  "
yearNewLine = intercalate "\n"

titleString :: Year -> String
titleString year =
    unpack . center 64 ' ' $ pack . strip . show $ year

spacerString =
  "                                                                "

monthNamePaddedArr :: [String]
monthNamePaddedArr =
  map (unpack . center 20 ' ' . pack) $ drop 1 months

threeMonthHeader monthRow = yearPadding $ take 3 $ drop ((monthRow - 1) * 3) monthNamePaddedArr

yearDaysOfWeek = yearPadding $ take 3 $ repeat daysString

pack20 :: String -> String
pack20 = unpack . justifyLeft 20 ' ' . pack . rstrip


threeMonthsWeekNumbers week monthRow year =
  yearPadding [packedChunks ((monthRow * 3) - 2)
              ,packedChunks ((monthRow * 3) - 1)
              ,packedChunks (monthRow * 3)]
  where packedChunks monthRow = pack20 $ chunks monthRow year !! (week - 1)


threeMonths :: Int -> Year -> String
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
