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
        "showDayInMonth":n:_ -> do
          let day = getDayFromMonthFromYear m (read n) (Just thisYear)
          let xs = showEventsFromDay day
          putStr $ concat xs
        "showDayInMonthInYear":x:u:w:_ -> do
          let p = getYearFromCalendar (read w) calendar 
          let day = getDayFromMonthFromYear (read u) (read x) p
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
          let Just y = addEventToYear e m (read x) thisYear
          putMyCalendar $ addYearToCalendar y calendar 
        "addEventWithTimeOnDayInMonth":h:m2:x:u:xs -> do
          let !h2 = timeCorrectionHour h
          let !m3 = timeCorrectionMin m2
          let e = EventWithTime (unwords xs) (h2++" "++m3)
          let Just y = addEventToYear e (read u) (read x) thisYear
          putMyCalendar $ addYearToCalendar y calendar
        "addEventOnDay":x:xs -> do
          let e = Event $ unwords xs
          let Just y = addEventToYear e m (read x) thisYear
          putMyCalendar $ addYearToCalendar y calendar
        "addEventOnDayInMonth":x:y:xs -> do
          let e = Event $ unwords xs
          let Just y2 = addEventToYear e (read y) (read x) thisYear
          putMyCalendar $ addYearToCalendar y2 calendar 
        "addEventOnDayInMonthInYear":x:u:w:xs -> do
          let e = Event $ unwords xs
          let p = getYearFromCalendar (read w) calendar
          case p of
            Nothing -> do
              let q = createYearWithInt (read w)
              let Just y2 = addEventToYear e (read u) (read x) q
              putMyCalendar $ addYearToCalendar y2 calendar
            Just q -> do 
              let Just y2 = addEventToYear e (read u) (read x) q
              putMyCalendar $ addYearToCalendar y2 calendar
        "removeEventToday":xs -> do 
          let e = Event $ unwords xs
          let y = removeEventFromYear e m d thisYear
          putMyCalendar $ addYearToCalendar y calendar 
        "removeEventOnDay":x:xs -> do
          let e = Event $ unwords xs
          let y = removeEventFromYear e m (read x) thisYear    
          putMyCalendar $ addYearToCalendar y calendar
        "removeEventOnDayInMonth":x:y:xs -> do
          let e = Event $ unwords xs
          let y2 = removeEventFromYear e (read y) (read x) thisYear
          putMyCalendar $ addYearToCalendar y2 calendar
        "removeEventOnDayInMonthInyear":x:u:w:xs -> do  
          let e = Event $ unwords xs
          let p = getYearFromCalendar (read w) calendar
          case p of 
            Nothing -> return ()
            Just q  -> do
              let y2 = removeEventFromYear e (read u) (read x) q
              putMyCalendar $ addYearToCalendar y2 calendar
        _ -> do
          putStrLn "unknown Command\nKnown Commands:"
          putStrLn "\tshowDay"
          putStrLn "\tshowMonth"
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



