module DateTime where

import ParseLib.Derived
import Prelude
import Data.Maybe

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
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseIsUtc

-- ParseDate includes a parsing of symbol 'T' but discards it as it is never use anywhere in the DateTime syntax
parseDate :: Parser Char Date
parseDate = (Date <$> (Year <$> parse4Digits) <*> (Month <$> parse2Digits) <*> (Day <$> parse2Digits)) <* symbol 'T'

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
run p xs =
      case getParserResult p xs of
        Nothing -> Nothing
        Just (b, _rest) -> Just b

getParserResult :: Parser a b -> [a] -> Maybe (b, [a])
getParserResult p xs = let
                  aList = parse p xs
                  in
                  listToMaybeHead aList

listToMaybeHead :: [(a, [s])] -> Maybe (a, [s])
listToMaybeHead [x] = Just x
listToMaybeHead _ = Nothing

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime (Date year month day) (Time hour minute second) utc) =
  showLen (runYear year) 4 ++ showLen (runMonth month) 2 ++ showLen (runDay day) 2 ++
  "T" ++
  showLen (runHour hour) 2 ++ showLen (runMinute minute) 2 ++ showLen (runSecond second) 2 ++
  (if utc then "Z" else "")

-- Version of show which adds leading 0s until length is matched
showLen :: Show a => a -> Int -> String
showLen toShow len = let
                        partialResult = show toShow
                        missingLen = len - length partialResult
                        result = replicate missingLen '0' ++ partialResult
                        in result

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = fmap printDateTime (run parseDateTime s)

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime (Date year month day) (Time hour minute second) utc) =
  (runYear year > -1) && (runMonth month > 0 && runMonth month < 13) && validDay (runMonth month) (runYear year) (runDay day) &&
  (runHour hour > -1 && runHour hour < 24) && (runMinute minute > -1 && runMinute minute < 60) && (runSecond second > -1 && runSecond second < 60)

-- Month, Year, Day
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

daysInMonth :: Year -> Month -> Int
daysInMonth (Year y) (Month m) =
  case m of
    1  -> 31
    2  -> if isLeap y then 29 else 28
    3  -> 31
    4  -> 30
    5  -> 31
    6  -> 30
    7  -> 31
    8  -> 31
    9  -> 30
    10 -> 31
    11 -> 30
    12 -> 31
    _  -> error "Invalid month"
  where
    isLeap yr =
      (yr `mod` 4 == 0 && yr `mod` 100 /= 0) || yr `mod` 400 == 0

dateToDays :: Date -> Int
dateToDays (Date (Year y) (Month m) (Day d)) =
    let
        yearsDays = y * 365 + y `div` 4 - y `div` 100 + y `div` 400
        monthsDays = sum [ daysInMonth (Year y) (Month m') | m' <- [1 .. m - 1] ]
    in yearsDays + monthsDays + (d - 1)

timeToMinutes :: Time -> Int
timeToMinutes (Time (Hour h) (Minute mi) (Second s)) =
    h * 60 + mi + s `div` 60

dateTimeToMinutes :: DateTime -> Int
dateTimeToMinutes (DateTime date time _utcFlag) =
    dateToDays date * 24 * 60 + timeToMinutes time
