module DateTime where

import ParseLib.Derived
import Prelude

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
parseDate = Date <$> (Year <$> parse4Digits) <*> (Month <$> parse2Digits) <*> (Day <$> parse2Digits)

parseTime :: Parser Char Time
parseTime = Time <$> (Hour <$> parse2Digits) <*> (Minute <$> parse2Digits) <*> (Second <$> parse2Digits)

parse4Digits :: Parser Char Int
parse4Digits = (\a b c d -> a * 1000 + b * 100 + c * 10 + d) <$> newdigit <*> newdigit <*> newdigit <*> newdigit

parse2Digits :: Parser Char Int
parse2Digits = (\a b -> a * 10 + b) <$> newdigit <*> newdigit

parseIsUtc :: Parser Char Bool
parseIsUtc = True <$ symbol 'Z'

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p xs = do
      (b, [a]) <- Just (head (parse p xs))
      Just b


-- Exercise 3
printDateTime :: DateTime -> String
printDateTime = undefined

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = fmap printDateTime (run parseDateTime s)

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
