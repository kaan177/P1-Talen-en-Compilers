module Features where

import DateTime
import Calendar

--Helpers
extractDateTimeFromEventProp :: EventProp -> DateTime
extractDateTimeFromEventProp (DTSTAMP d) = d
extractDateTimeFromEventProp (DTSTART d) = d
extractDateTimeFromEventProp (DTEND   d) = d
--Evenything else is an error :)

extractStringFromEventProp :: EventProp -> String
extractStringFromEventProp (UID value) = value
extractStringFromEventProp (SUMMARY value) = value
extractStringFromEventProp (DESCRIPTION value) = value
extractStringFromEventProp (LOCATION value) = value
--Everything else is an error :)

getEventProp :: Event -> String -> EventProp
getEventProp (Event eventProps) name = let matchedEvents = filter (\x -> showEventPropHeader x == name) eventProps -- In all usecases this is a singleton list
                                        in head matchedEvents -- Only used in situations where we know this will not crash :)

eventGetStartEnd :: Event -> (DateTime, DateTime)
eventGetStartEnd event = let 
                            start = extractDateTimeFromEventProp $ getEventProp event "DTSTART"
                            end   = extractDateTimeFromEventProp $ getEventProp event "DTEND"
                            in (start, end)

timeInEvent :: Event -> DateTime -> Bool
timeInEvent event dt = let (start, end) = eventGetStartEnd event
                        in start <= dt && dt < end --Note end is exclusive

-- This checks both ways
eventOverlaps :: Event -> Event -> Bool
eventOverlaps event1 event2 = let 
                        (s1, e1) = eventGetStartEnd event1
                        (s2, e2) = eventGetStartEnd event2
                        in timeInEvent event2 s1 || timeInEvent event2 e1 || timeInEvent event1 s2 || timeInEvent event1 e2

-- Exercise 9
countEvents :: Calendar -> Int
countEvents (Calendar _ events) = length events

--NOTE: All these functions compare DateTime using the < <= > >= operators, this should work as they derive ord
--If this does not work, run printDateTime and then do the same thing, the strings are in order

findEvents :: DateTime -> Calendar -> [Event]
findEvents dt (Calendar _ events) = filter (`timeInEvent` dt) events

checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ events) = eventsOverlapping' events

--Helper for eventsOverlapping
eventsOverlapping' :: [Event] -> Bool
eventsOverlapping' []     = False
eventsOverlapping' (e:es) = any (eventOverlaps e) es || eventsOverlapping' es

--The function requirement is ambiguous, so interpreting as use find events and then count amount of time spent. Not as subtracting overlapping times
timeSpent :: String -> Calendar -> Int
timeSpent summary calendar =
    let matchedEvents = findEventsByProperty "SUMMARY" summary calendar
    in sum (map eventLength matchedEvents)

--This is a helper for timeSpent
findEventsByProperty :: String -> String -> Calendar -> [Event]
findEventsByProperty eventProperty string (Calendar _ events) = filter (\x -> extractStringFromEventProp (getEventProp x eventProperty) == string) events

eventLength :: Event -> Int
eventLength event =
    let (start, end) = eventGetStartEnd event
    in dateTimeToMinutes end - dateTimeToMinutes start
