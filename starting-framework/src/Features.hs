module Features where

import DateTime
import Calendar


-- Exercise 9
countEvents :: Calendar -> Int
countEvents (Calender _ _ events) = length events

findEvents :: DateTime -> Calendar -> [Event]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined
