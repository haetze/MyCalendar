--Richard Stewing
--
--11/04/2015
--
--Schedule.hs
-- 
--This file is the starting point for the weekly schedule implemantion


module Schedule where


import Data.List
import System.IO


--Time
--First Int is the hours and the second Int is the minutes 
--24h clock !!
--The minutes will be in steps of 30 minutes 
--
data Time = Time (Int, Int)
  deriving(Show, Read, Eq)


--event
--the name of the event should be short (ca. 4 charcacters)
--hold an event, name, start time, end time
data Event = Event String Time
  deriving(Show, Read, Eq)

--Day 
--holds the name of the day (Monday....Sunday) and a list of Events on that
--day
type Day = (String, [Event])

--Week 
--a list of days
data Week = Week [Day]

--Ord instances for Time and therefore Event
instance Ord Time where
  (Time (x, y)) >  (Time (u, t)) | x == u = y > t
                | otherwise = x > u
  (Time (x, y)) < (Time (u, t)) | x == u = y < t
                | otherwise = x < u
  (Time (x, y)) >= (Time (u, t)) | x == u = y >= t
                | otherwise = x >= u
  (Time (x, y)) <= (Time (u, t)) | x == u = y <= t
                | otherwise = x <= u

instance Ord Event where
  (Event _ t) < (Event _ u) = t < u
  (Event _ t) > (Event _ u) = t > u
  (Event _ t) <= (Event _ u) = t <= u
  (Event _ t) >= (Event _ u) = t >= u
--End Ord instance 

--Print for Time

toString:: Time -> String
toString (Time (n, u)) = show n ++ ":" ++ show u

nextTime:: Time -> Time
nextTime (Time (23,30))= Time (0, 0)
nextTime (Time (x, 0)) = Time (x, 30)
nextTime (Time (x, 30)) = Time (x + 1, 0)

createDay:: String -> IO Day
createDay x = do
  u <- createEventList (Time (0, 0))
  return (x, u)

createEventList::Time -> IO [Event]
createEventList t@(Time (x, y)) = do
  case t of 
    Time (23, 30) -> do
      e <- createEventAt t 
      return [e]
    _ -> do 
      e <- createEventAt t
      es <- createEventList (nextTime t) 
      return (e:es)


createEventAt:: Time -> IO Event
createEventAt t = do
  putStr $ "Event at " ++ toString t ++ ":"
  name <- getLine 
  case length name of
    0 -> return $ Event name t --I later want to change this behavior
    _ -> return $ Event name t 

createWeek::IO Week 
createWeek = do
  monday <- createDay "Monday"
  tuesday <- createDay "Tuesday"
  wednesday <- createDay "Wednesday"
  thursday <- createDay "Thursday"
  friday <- createDay "Friday"
  saturday <- createDay "Saturday"
  sunday <- createDay "Sunday"
  return $ Week [monday,tuesday,wednesday,thursday,friday,saturday,sunday] 










