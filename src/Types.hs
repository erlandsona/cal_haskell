module Types where

type MonthRow = Int
data Month
    = Null
    | January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
    deriving (Enum, Eq, Ord, Show)

months :: [Month]
months = enumFrom January

newtype Day = Day Int deriving (Eq, Show)
newtype Year = Year Int
newtype DayOfWeek = DayOfWeek Int deriving (Eq, Show)
newtype Week = Week Int
