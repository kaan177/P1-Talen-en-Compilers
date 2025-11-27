module Features where

import DateTime
import Calendar

--Helpers
extractDateTimeFromEventProp :: EventProp -> DateTime
extractDateTimeFromEventProp (DTSTAMP d) = d
extractDateTimeFromEventProp (DTSTART d) = d
extractDateTimeFromEventProp (DTEND   d) = d
--Evenything else is an error :)

getEventProp :: Event -> String -> EventProp
getEventProp (Event eventProps) name = let matchedEvents = filter (\x -> showEventPropHeader x == name) eventProps -- In all usecases this is a singleton list
                                        in head matchedEvents -- Only used in situations where we know this will not crash :)

eventGetStartEnd :: Event -> (DateTime, DateTime)
eventGetStartEnd event = let 
                            start = extractDateTimeFromEventProp $ getEventProp event "DTSTART"
                            end   = extractDateTimeFromEventProp $ getEventProp event "DTEND"
                            in (start, end)

eventMatchesTime :: Event -> DateTime -> Bool
eventMatchesTime event dt = undefined

-- Exercise 9
countEvents :: Calendar -> Int
countEvents (Calendar _ events) = length events

findEvents :: DateTime -> Calendar -> [Event]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined
