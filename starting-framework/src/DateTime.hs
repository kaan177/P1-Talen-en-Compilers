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
printDateTime (DateTime (Date year month day) (Time hour minute second) utc) =
  show (runYear year) ++ show (runMonth month) ++ show (runDay day) ++
  "T" ++
  show (runHour hour) ++ show (runMinute minute) ++ show (runSecond second) ++
  (if utc then "Z" else "")

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = fmap printDateTime (run parseDateTime s)

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime (Date year month day) (Time hour minute second) utc) =
  (runYear year > -1) && (runMonth month > 0 && runMonth month < 13) && validDay (runYear year) (runMonth month) (runDay day) &&
  (runHour hour > 0 && runHour hour < 24) && (runMinute minute > 0 && runMinute minute < 60) && (runSecond second > 0 && runSecond second < 60) 


validDay :: Int -> Int -> Int -> Bool
validDay 1  _ d = d > 0 && d <= 31
validDay 2  y d = d > 0 && d <= (if isLeapYear y then 29 else 28)
validDay 3  _ d = d > 0 && d <= 31
validDay 4  _ d = d > 0 && d <= 30
validDay 5  _ d = d > 0 && d <= 31
validDay 6  _ d = d > 0 && d <= 30
validDay 7  _ d = d > 0 && d <= 31
validDay 8  _ d = d > 0 && d <= 31
validDay 9  _ d = d > 0 && d <= 30
validDay 10 _ d = d > 0 && d <= 31
validDay 11 _ d = d > 0 && d <= 30
validDay 12 _ d = d > 0 && d <= 31
validDay _  _ _ = False

isLeapYear :: Int -> Bool
isLeapYear y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0)

