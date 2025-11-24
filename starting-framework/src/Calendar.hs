module Calendar where

import DateTime
import GHC.Generics (Generic)
import GradeLib.CustomASTData (CustomData)
import ParseLib.Derived

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
    description :: String, --Optional
    summary :: String,     --Required
    location :: String     --Optional
  }
  deriving (Eq, Ord, Show)

-- If you plan on using your own types in Calendar, Event, or Token. Make sure it derives Eq, Generic, and CustomData.
-- Example:
-- data ExampleCustomData = ExampleCustomData
--   deriving (Eq, Ord, Show, Generic, CustomData)

-- Exercise 7
data Token = Token
  deriving (Eq, Ord, Show)

lexCalendar :: Parser Char [Token]
lexCalendar = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

parseCalendar' :: String -> Maybe Calendar
parseCalendar' s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
