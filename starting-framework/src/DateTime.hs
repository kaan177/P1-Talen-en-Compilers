module DateTime where

import ParseLib.Derived

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime
  { date :: Date,
    time :: Time,
    utc :: Bool
  }
  deriving (Eq, Ord, Show)

data Date = Date
  { year :: Year,
    month :: Month,
    day :: Day
  }
  deriving (Eq, Ord, Show)

newtype Year = Year {runYear :: Int} deriving (Eq, Ord, Show)

newtype Month = Month {runMonth :: Int} deriving (Eq, Ord, Show)

newtype Day = Day {runDay :: Int} deriving (Eq, Ord, Show)

data Time = Time
  { hour :: Hour,
    minute :: Minute,
    second :: Second
  }
  deriving (Eq, Ord, Show)

newtype Hour = Hour {runHour :: Int} deriving (Eq, Ord, Show)

newtype Minute = Minute {runMinute :: Int} deriving (Eq, Ord, Show)

newtype Second = Second {runSecond :: Int} deriving (Eq, Ord, Show)

-- Exercise 1
-- type Parser Char DateTime = [Char] → [(DateTime, [Char])]
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseIsUtc

parseDate :: Parser Char Date
parseDate = undefined

parseTime :: Parser Char Time
parseTime = undefined

parseIsUtc :: Parser Char Bool
parseIsUtc = undefined

isUtc :: [Char] -> Bool
isUtc (x:_) = x == 'Z'
isUtc = False

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run = undefined

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime = undefined

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = fmap printDateTime (run parseDateTime s)

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
