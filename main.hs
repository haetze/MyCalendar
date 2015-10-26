#! /usr/bin/env runhugs +l
--
-- main.hs
-- Copyright (C) 2015 haetze <haetze@Richards-MacBook-Air.local>
--
-- Distributed under terms of the MIT license.
--
{-#LANGUAGE BangPatterns #-}

import System.Environment(getArgs)
import System.IO
import Calender 
import Control.Exception
import System.Directory

main = handle f pro
  where 
    f::SomeException -> IO ()  
    f x =do 
       case show x of 
        "Wrong time format" ->
          putStrLn $ "Your time looks fishy, check that and try again."
        "Prelude.read: no parse" -> 
            putStrLn "Your input appears to be wrong formatted, run showDay to check the integraty of your ~/.calendar"
        _ -> do  
          (putStrLn $ "some error happend, run showDay and check if the program still crashes if yes your calendar is corrupted, delete it!")
          --c <- createYear
          --putMyCalendar $ Calendar [c]
  

pro = do
  !calendar <- checkCalender
  y   <- year
  m   <- month
  d    <- today
  args <- getArgs
  let t = getYearFromCalendar y calendar 
  case t of
    Nothing -> do
      newYear <- createYear
      putMyCalendar $ addYearToCalendar newYear calendar
      main
    Just thisYear -> do   
      case args of
        "showMonth":_ -> do
          let thisMonth = getMonthFromYear m thisYear
          let xs = showEventsFromMonth thisMonth
          putStr $ concat xs
        "showDay":_ -> do
          let day = getDayFromMonthFromYear m d (Just thisYear) 
          let xs = showEventsFromDay day
          putStr $ concat xs
        "showMonthNumber":n:_ -> do
          let !n2 = read n
          let thisMonth = getMonthFromYear n2 thisYear
          let xs = showEventsFromMonth thisMonth
          putStr $ concat xs 
        "showDayInMonth":n:_ -> do
          let !n2 = (read n)
          let day = getDayFromMonthFromYear m n2 (Just thisYear)
          let xs = showEventsFromDay day
          putStr $ concat xs
        "showDayInMonthInYear":x:u:w:_ -> do
          let !n2 = (read w)
          let !n3 = (read u)
          let !n4 = (read x)
          let p = getYearFromCalendar n2 calendar 
          let day = getDayFromMonthFromYear n3 n4 p
          let xs = showEventsFromDay day
          putStr $ concat xs
        "addEventToday":xs -> do
          let e = Event $ unwords xs
          let Just y = addEventToYear e m d thisYear
          putMyCalendar $ addYearToCalendar y calendar
        "addEventWithTimeToday":h:m2:xs -> do
          let !h2 = timeCorrectionHour h
          let !m3 = timeCorrectionMin m2
          let e = EventWithTime (unwords xs) (h2++" "++m3)
          let Just y = addEventToYear e m d thisYear
          putMyCalendar $ addYearToCalendar y calendar
        "addEventWithTimeOnDay":h:m2:x:xs -> do
          let !h2 = timeCorrectionHour h
          let !m3 = timeCorrectionMin m2
          let e = EventWithTime (unwords xs) (h2++" "++m3)
          let !n2 = (read x)
          let Just y = addEventToYear e m n2 thisYear
          putMyCalendar $ addYearToCalendar y calendar 
        "addEventWithTimeOnDayInMonth":h:m2:x:u:xs -> do
          let !h2 = timeCorrectionHour h
          let !m3 = timeCorrectionMin m2
          let e = EventWithTime (unwords xs) (h2++" "++m3)
          let !n3 = (read x)
          let !n2 = (read u)
          let Just y = addEventToYear e n2 n3 thisYear
          putMyCalendar $ addYearToCalendar y calendar
        "addEventOnDay":x:xs -> do
          let e = Event $ unwords xs
          let !n2 = (read x)
          let Just y = addEventToYear e m n2 thisYear
          putMyCalendar $ addYearToCalendar y calendar
        "addEventOnDayInMonth":x:y:xs -> do
          let e = Event $ unwords xs
          let !n2 = (read y)
          let !n3 = (read x)
          let Just y2 = addEventToYear e n2 n3 thisYear
          putMyCalendar $ addYearToCalendar y2 calendar 
        "addEventOnDayInMonthInYear":x:u:w:xs -> do
          let e = Event $ unwords xs
          let !n2 = (read w)
          let p = getYearFromCalendar n2 calendar
          case p of
            Nothing -> do
              let !n2 = (read w)
              let q = createYearWithInt n2 
              let !n3 = (read u)
              let !n4 = (read x)
              let Just y2 = addEventToYear e n3 n4 q
              putMyCalendar $ addYearToCalendar y2 calendar
            Just q -> do 
              let !n2 = (read u)
              let !n3 = (read x)
              let Just y2 = addEventToYear e n2 n3 q
              putMyCalendar $ addYearToCalendar y2 calendar
        "removeEventToday":xs -> do 
          let e = Event $ unwords xs
          let y = removeEventFromYear e m d thisYear
          putMyCalendar $ addYearToCalendar y calendar 
        "removeEventOnDay":x:xs -> do
          let e = Event $ unwords xs
          let !n2 = read x
          let y = removeEventFromYear e m n2 thisYear    
          putMyCalendar $ addYearToCalendar y calendar
        "removeEventOnDayInMonth":x:y:xs -> do
          let e = Event $ unwords xs
          let !n2 = (read y)
          let !n3 = (read x)
          let y2 = removeEventFromYear e n2 n3 thisYear
          putMyCalendar $ addYearToCalendar y2 calendar
        "removeEventOnDayInMonthInyear":x:u:w:xs -> do  
          let e = Event $ unwords xs
          let !n2 = read w
          let p = getYearFromCalendar n2 calendar
          case p of 
            Nothing -> return ()
            Just q  -> do
              let !n2 = (read u)
              let !n3 = (read x)
              let y2 = removeEventFromYear e n2 n3 q
              putMyCalendar $ addYearToCalendar y2 calendar
        _ -> do
          putStrLn "unknown Command\nKnown Commands:"
          putStrLn "\tshowDay"
          putStrLn "\tshowMonth"
          putStrLn "\tshowMonthNumber <Month #>"
          putStrLn "\tshowDayInMonth <Day #>"
          putStrLn "\tshowDayInMonthInYear <Day #> <Month #> <Year #>"
          putStrLn "\taddEventToday <Event title, can be more than one word>"
          putStrLn "\taddEventOnDay <Day #> <Event title, can be more than one word>"
          putStrLn "\taddEventOnDayInMonth <Day #> <Month #> <Event title, can be more than one word>"
          putStrLn "\taddEventOnDayInMonthInYear <Day #> <Month #> <Event title, can be more than one word>"
          putStrLn "\taddEventWithTimeToday <Hour> <Minutes> <Event title, can be more than one word>"
          putStrLn "\taddEventWithTimeOnDay <Hour> <Minutes> <Day #> <Event title, can be more than one word>"
          putStrLn "\taddEventWithTimeOnDay <Hour> <Minutes> <Day #> <Month #> <Event title, can be more than one word>"
          putStrLn "\tremoveEventToday <Event title, can be more than one word>"
          putStrLn "\tremoveEventOnDay <Day #> <Event title, can be more than one word>"
          putStrLn "\tremoveEventOnDayInMonth <Day #> <Month #> <Event, title, can be more than one word>"
          putStrLn "\tremoveEventOnDayInMonthInyear <Day #> <Month #> <Year #> <Event title, can be more than one word>"
          putStrLn "\tBug report? email to richy.sting@gmail.com"



