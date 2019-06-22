module Types where

type MonthRow = Int
newtype Month = Month Int
newtype Day = Day Int deriving (Eq, Show)
newtype Year = Year Int
newtype DayOfWeek = DayOfWeek Int deriving (Eq, Show)
newtype Week = Week Int
