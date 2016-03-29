module Main where

import System.Environment
import Month
import Year

-- getIntArgs = fmap (read . head) getArgs

main = do
  args <- getArgs

  if (read $ head args) > 12
    then do putStrLn $ "Please enter arguments in the format `cal [MM] [YYYY]`\nwhere month is between 1..12 and year is between 0..9999"
  else if (read $ args !! 1) > 9999
    then do putStrLn $ "Please enter arguments in the format `cal [MM] [YYYY]`\nwhere month is between 1..12 and year is between 0..9999"
  else if any (<0) $ map (read) args
    then do putStrLn $ "Please enter arguments in the format `cal [MM] [YYYY]`\nwhere month is between 1..12 and year is between 0..9999"
  else if (length args) > 1
    then do putStrLn $ monthString (read $ args !! 0) (read $ args !! 1)
  else if (length args) == 1
    then do putStrLn $ yearString (read $ args !! 0)
  else do putStrLn $ yearString 2016
