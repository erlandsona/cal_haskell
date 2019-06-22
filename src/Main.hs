import System.Environment (getArgs)

import Month (monthString)
import Year (yearString)
import Types

main :: IO ()
main = do
  args <- map read <$> getArgs -- TODO: read will explode on non-numeric input
  putStrLn $ case args of
    y  :[] | isYear y             -> yearString (Year y)
    m:y:_ | isMonth m && isYear y -> monthString (Month m) (Year y)
    _
      -> "Please enter arguments in the format `cal [MM] [YYYY]`\n where month \
          \is between 1..12 and year is between 0..9999"
 where
  isYear :: Int -> Bool
  isYear y = y >= 0 && y <= 9999

  isMonth :: Int -> Bool
  isMonth m = m >= 1 && m <= 12
