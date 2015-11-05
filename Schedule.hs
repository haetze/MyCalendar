--Richard Stewing
--
--11/04/2015
--
--Schedule.hs
-- 
--This file is the starting point for the weekly schedule implemantion

{-#LANGUAGE BangPatterns #-}

module Schedule (
  createWeek,
  checkWeek,
  putMyWeek,
  getMyWeek,
  printWeekFromTo,
  printWeek,
  createTime,
  createDay,
  createEventList,
  createEventAt
) where


import System.Directory
import Data.List
import System.IO


--Time
--First Int is the hours and the second Int is the minutes 
--24h clock !!
--The minutes will be in steps of 30 minutes 
--
data Time = Time (Int, Int)
  deriving(Show, Read, Eq)

createTime::Int -> Int -> Time
createTime x y = Time (x, y)


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
  deriving(Read, Show)

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

toStringEvent:: Event -> String
toStringEvent (Event n _) = n

nextTime:: Time -> Time
nextTime (Time (23,30))= Time (0, 0)
nextTime (Time (x, 0)) = Time (x, 30)
nextTime (Time (x, 30)) = Time (x + 1, 0)

createDay:: String -> IO Day
createDay x = do
  !u <- createEventList (Time (0, 0))
  return (x, u)

createEventList::Time -> IO [Event]
createEventList t@(Time (x, y)) = do
  case t of 
    Time (23, 30) -> do
      e <- createEventAt t 
      return [e]
    _ -> do 
      !e <- createEventAt t
      !es <- createEventList (nextTime t) 
      return (e:es)


createEventAt:: Time -> IO Event
createEventAt t = s >> y >>= f 
  where
  s = do  
    putStr $ "Event at " ++ toString t ++ ":"
    hFlush stdout
  y = getLine
  f name  = case length name of
    0 -> return $ Event name t --I later want to change this behavior
    _ -> case length name < 6 of
      True -> return $ Event name t
      False -> return $ Event (take 5 name) t  

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

printWeekFromTo::Week -> Time -> Time -> IO ()
printWeekFromTo (Week days) t1 t2 = do
  putStr "00:00\tM   \tT   \tW   \tTh \tF  \tS   \tSu\n"
  let x = createLines days t1 t2 
  putStr $ concat x



printWeek:: Week -> IO ()
printWeek (Week days) = do
  putStr "00:00\tM   \tT   \tW   \tTh \tF  \tS   \tSu\n"
  let !x = createLines days (Time (0, 0)) (Time (23, 30))
  putStr $ concat x

createLines:: [Day] ->Time -> Time -> [String]
createLines days t1 t2 = createLinesFromTime days t1 t2

createLinesFromTime:: [Day] -> Time -> Time -> [String]
createLinesFromTime days t1 t2 | t1 == t2 = q:[]
  where
  q = toString t1 ++ "\t" ++ timeLine days t1
createLinesFromTime days t1 t3 = q:u
  where
  q = toString t1 ++ "\t" ++ timeLine days t1  
  u = createLinesFromTime days t2 t3
  t2 = nextTime t1


timeLine:: [Day] -> Time -> String 
timeLine ds t = (concat $ map (eventAtTime t) ds) ++ "\n"

eventAtTime::Time -> Day -> String
eventAtTime _ (_, []) = ""
eventAtTime t (m, (Event n z):es) | t == z = n ++ "\t" 
                                  | otherwise = eventAtTime t (m, es)


--File Read Write Area 
-- ==========================================
--
--
--
--
--
--
--
--

getMyWeek:: IO Week
getMyWeek = do
  !home <- getHomeDirectory
  let file = home ++ "/.week"
  !contents <- readFile file
  let r = read contents
  return (r)

putMyWeek::Week -> IO ()
putMyWeek week = do
  !home <- getHomeDirectory
  let file = home ++ "/.week"
  writeFile file $ show week


checkWeek:: IO Week
checkWeek = getHomeDirectory >>= getDirectoryContents >>= isElem2

isElem2::[FilePath] -> IO Week
isElem2 xs = do 
  case ".week" `elem` xs of
    True -> getMyWeek
    False -> do
      week <- createWeek
      putMyWeek $ week
      return $ week

--File Read Write Area End











