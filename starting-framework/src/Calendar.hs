{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Calendar where

import DateTime
import GHC.Generics (Generic)
import GradeLib.CustomASTData (CustomData)
import ParseLib.Derived
import Data.Char as Char
import Control.Applicative ((<|>))
import Data.Functor

-- Exercise 6
data Calendar = Calendar
  {
    version :: String,
    prodID :: String,
    events :: [Event]
  }
  deriving (Eq, Ord, Show)

-- Even is a data type that has a few required entries and a few optional ones. The order of the contents does not matter to the event
data Event = Event
  {
    stampDate :: DateTime, --Required
    uid :: String,         --Required
    startDate :: DateTime, --Required
    endDate :: DateTime,   --Required
    summary :: String,     --Required
    description :: String, --Optional
    location :: String     --Optional
  }
  deriving (Eq, Ord, Show)

-- If you plan on using your own types in Calendar, Event, or Token. Make sure it derives Eq, Generic, and CustomData.
-- Example:
-- data ExampleCustomData = ExampleCustomData
--   deriving (Eq, Ord, Show, Generic, CustomData)

-- Exercise 7
data Token = Token
  {
    header :: Header,
    content :: Content
  }
  deriving (Eq, Ord, Show)

data Header = PRODID | VERSION | EVENT | DTSTAMP | UID | DTSTART | DTEND | SUMMARY | DESCRIPTION | LOCATION deriving (Enum, Eq, Ord, Show, Generic, CustomData)
-- List of token is for storing an event
data Content = Timestamp DateTime | String String | Tokens [Token] deriving (Eq, Ord, Show, Generic, CustomData)

-- Note cannot retreive EVENT enum from get header, as not done through string
getHeader :: String -> Header
getHeader "PRODID" = PRODID
getHeader "VERSION" = VERSION
getHeader "DTSTAMP" = DTSTAMP
getHeader "UID" = UID
getHeader "DTSTART" = DTSTART
getHeader "DTEND" = DTEND
getHeader "SUMMARY" = SUMMARY
getHeader "DESCRIPTION" = DESCRIPTION
getHeader "LOCATION" = LOCATION

newline :: Parser Char Char
newline = symbol '\n'

parseBeforeColon :: Parser Char String
parseBeforeColon = greedy (satisfy (/=':'))

colon :: Parser Char Char
colon = symbol ':'

parseHeader :: Parser Char Header
parseHeader = getHeader <$> some (satisfy Char.isAlpha)

parseContent :: Header -> Parser Char Content
parseContent header =
  case header of
    DTSTAMP -> Timestamp <$> parseDateTime
    DTSTART -> Timestamp <$> parseDateTime
    DTEND   -> Timestamp <$> parseDateTime
    _       -> String   <$> greedy (satisfy (/='\n'))

parseLine :: Parser Char Token
parseLine = do
  key <- parseBeforeColon
  _   <- colon
  let header = getHeader key
  content   <- parseContent header
  _   <- newline
  pure (Token header content)

parseEvent :: Parser Char Token
parseEvent = do
  _ <- token "BEGIN:VEVENT" *> newline
  tokens <- many parseLine
  _ <- token "END:VEVENT" *> newline
  pure (Token EVENT (Tokens tokens))

lexCalendar :: Parser Char [Token]
lexCalendar = do
  _ <- token "BEGIN:VCALENDAR" *> newline
  tokens <- many (parseEvent <|> parseLine)
  _ <- ((token "END:VCALENDAR" *> newline) $> ()) <|> (token "END:VCALENDAR" $> ())
  pure tokens


parseCalendar :: Parser Token Calendar
parseCalendar = undefined

parseCalendar' :: String -> Maybe Calendar
parseCalendar' s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
