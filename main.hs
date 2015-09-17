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
--import qualified System.Time as T

main = do
	!calendar <- getMyCalendar
	y	 <- year
	m	 <- month
	d 	 <- today
	args <- getArgs
	let Just thisYear = getYearFromCalendar y calendar 
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
		"addEventToday":xs -> do
			let e = Event $ unwords xs
			let Just y = addEventToYear e m d thisYear
			putMyCalendar $ Calendar [y]
		"addEventOnDay":xs -> do
			let n = last xs
			let e = Event $ unwords . reverse . tail $ reverse xs
			let Just y = addEventToYear e m (read n) thisYear
			putMyCalendar $ Calendar [y]
		"removeEventToday":xs -> do 
			let e = Event $ unwords xs
			let y = removeEventFromYear e m d thisYear
			putMyCalendar $ Calendar [y]

		"removeEventOnDay":xs -> do
			let n = last xs
			let e = Event $ unwords . reverse . tail $ reverse xs
			let y = removeEventFromYear e m (read n) thisYear		
			putMyCalendar $ Calendar [y]
	


