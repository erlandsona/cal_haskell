module Year where

import Data.List
import Data.Text (center, pack, unpack)
import Data.String.Utils (strip)
import Month

type Year = Int

titleString year =
    unpack
   .center 64 ' ' $
      pack
      .strip
      .show $
        year


spacerString =
  "                                                                "

monthNamePaddedArr :: [String]
monthNamePaddedArr =
  map (unpack . center 20 ' ' . pack) $ drop 1 months


twelveMonths :: Year -> String
twelveMonths year =
  ""

yearString :: Year -> String
yearString year =
  intercalate "\n"
    [titleString year
    ,spacerString
    ,twelveMonths year]
