module Year where

import Data.Text (center, pack, unpack)
import Data.String.Utils (strip)
import Month


titleString year =
    unpack
   .center 64 ' ' $
      pack
      .strip
      .show $
        year


spacerString =
  "                                                                "

monthNameZipper = undefined
