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
    eventprops :: [EventProp]
  }
  deriving (Eq, Ord, Show)

data EventProp = DTSTAMP DateTime | UID String | DTSTART DateTime | DTEND DateTime | SUMMARY String | DESCRIPTION String | LOCATION String deriving (Eq, Ord, Show)
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

newline :: Parser Char Char
newline = symbol '\n'

parseBeforeColon :: Parser Char String
parseBeforeColon = greedy (satisfy (/=':'))

colon :: Parser Char Char
colon = symbol ':'

lexHeader :: Parser Char Header
lexHeader = Header <$> some (satisfy Char.isAlpha)

lexContent :: Header -> Parser Char Content
lexContent (Header header) =
  case header of
    "DTSTAMP" -> Timestamp <$> parseDateTime
    "DTSTART" -> Timestamp <$> parseDateTime
    "DTEND"   -> Timestamp <$> parseDateTime
    _       -> String   <$> greedy (satisfy (/='\n'))

lexLine :: Parser Char Token
lexLine = do
  key <- parseBeforeColon
  _   <- colon
  let header = Header key
  content   <- lexContent header
  _   <- newline
  pure (Token header content)

lexEvent :: Parser Char Token
lexEvent = do
  _ <- token "BEGIN:VEVENT" *> newline
  tokens <- many lexLine
  _ <- token "END:VEVENT" *> newline
  pure (Token (Header "EVENT") (Tokens tokens))

lexCalendar :: Parser Char [Token]
lexCalendar = do
  _ <- token "BEGIN:VCALENDAR" *> newline
  tokens <- many (lexEvent <|> lexLine)
  _ <- ((token "END:VCALENDAR" *> newline) $> ()) <|> (token "END:VCALENDAR" $> ())
  pure tokens


parseCalendar :: Parser Token Calendar
parseCalendar = undefined

parseCalendar' :: String -> Maybe Calendar
parseCalendar' s = run lexCalendar s >>= run parseCalendar

printCalProp :: CalProp -> String
printCalProp (VERSION v) = "VERSION:" ++ v
printCalProp (PRODID p ) = "PRODID:"  ++ p

printEventProp :: EventProp -> String
printEventProp (DTSTAMP     d) = "DTSTAMP:"     ++ printDateTime d
printEventProp (DTSTART     d) = "DTSTART:"     ++ printDateTime d
printEventProp (DTEND       d) = "DTEND:"       ++ printDateTime d
printEventProp (UID         u) = "UID:"         ++ u
printEventProp (SUMMARY     s) = "SUMMARY:"     ++ s
printEventProp (DESCRIPTION d) = "DESCRIPTION:" ++ d
printEventProp (LOCATION    l) = "LOCATION:"    ++ l

printEvent :: Event -> String
printEvent (Event eventProps) = "BEGIN:VEVENT\n" ++ undefined ++ "END:VEVENT\n"

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar (Calendar calProps events) = "BEGIN:VCALENDER\n" ++ unwords (map printCalProp calProps) ++ unwords (map printEvent events) ++ "END:VCALENDER"
