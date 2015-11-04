--Richard Stewing
--
--11/04/2015
--
--Schedule.hs
-- 
--This file is the starting point for the weekly schedule implemantion


module Schedule where


import Data.List


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
data Event = Event String Time Time
  deriving(Show, Read, Eq)

--Day 
--holds the name of the day (Monday....Sunday) and a list of Events on that
--day
type Day = (String, [Event])

--Week 
--a list of days
type Week = [Day]

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
  (Event _ t _ ) < (Event _ u _) = t < u
  (Event _ t _ ) > (Event _ u _) = t > u
  (Event _ t _ ) <= (Event _ u _) = t <= u
  (Event _ t _ ) >= (Event _ u _) = t >= u
--End Ord instance 

