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
    calprops :: [CalProp],
    events :: [Event]
  }
  deriving (Eq, Ord, Show)

data CalProp = PRODID String | VERSION String deriving (Eq, Ord, Show)
-- Even is a data type that has a few required entries and a few optional ones. The order of the contents does not matter to the event
data Event = Event
  {
    eventprops :: [EventProps]
  }
  deriving (Eq, Ord, Show)

data EventProps = DTSTAMP DateTime | UID String | DTSTART DateTime | DTEND DateTime | SUMMARY String | DESCRIPTION String | LOCATION String
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

newtype Header = Header String deriving ( Eq, Ord, Show, Generic, CustomData)
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

showHeader :: Header -> String
showHeader PRODID = "PRODID"
showHeader VERSION = "VERSION"
showHeader DTSTAMP = "DTSTAMP"
showHeader UID = "UID" 
showHeader DTSTART = "DTSTART"
showHeader DTEND = "DTEND"
showHeader SUMMARY = "SUMMARY"
showHeader DESCRIPTION = "DESCRIPTION"
showHeader LOCATION = "LOCATION"

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

printContent :: Content -> String
printContent content =
          case content of
            Timestamp content -> printDateTime content
            String string -> show string
            Tokens tokens -> printEvent tokens


printToken :: Token -> String
printToken (Token header content) = printHeader header ++ printContent content ++ "\n"

printEvent :: [Token] -> String
printEvent = "BEGIN:VEVENT\n" ++ map printToken ++ "END:VEVENT\n"
--printEvent (Event stampDate uid startDate endDate summary description location) = undefined

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar (Calendar version prodID events) = "BEGIN:VCALENDER\n" ++ "VERSION:" ++ version ++ "\nPRODID:" ++ prodID ++ "\n" ++ map printEvent events ++ "END:VCALENDER"
