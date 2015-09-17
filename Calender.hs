#! /usr/bin/env runhugs +l
--
-- Calender.hs
-- Copyright (C) 2015 haetze <haetze@Richards-MacBook-Air.local>
--
-- Distributed under terms of the MIT license.
--

module Calender where

import qualified System.Time as T
import System.Directory
import System.IO

type Title = String

data Event = Event Title |
	EventDue Title
	deriving(Show, Read, Eq)

data Day = Day Int [Event]
	deriving(Show, Read, Eq)

data Month = Month Int [Day]
	deriving(Show, Read, Eq)

data Year = Year Int [Month]
	deriving(Show, Read, Eq)

data Calendar = Calendar [Year]
	deriving(Show, Read, Eq)

createYearWithInt::Int -> Year
createYearWithInt x = Year x []

createYear:: IO Year
createYear = do
	c <- T.getClockTime
	cal <- T.toCalendarTime c
	return (Year (T.ctYear cal) [])


today:: IO Int 
today = do 
	c <- T.getClockTime
	cal <- T.toCalendarTime c
	return $ T.ctDay cal

year:: IO Int 
year = do 
	c <- T.getClockTime
	cal <- T.toCalendarTime c
	return $ T.ctYear cal

month:: IO Int 
month = do 
	c <- T.getClockTime
	cal <- T.toCalendarTime c
	return . toMonthNumber $ T.ctMonth cal






createMonthWithInt::Int -> Month
createMonthWithInt x = Month x []

createMonth:: IO Month 
createMonth = do
	c <- T.getClockTime
	cal <- T.toCalendarTime c
	return (Month (toMonthNumber $ T.ctMonth cal) [])	

toMonthNumber:: T.Month -> Int
toMonthNumber T.January = 1
toMonthNumber T.February = 2
toMonthNumber T.March = 3
toMonthNumber T.April = 4
toMonthNumber T.May = 5
toMonthNumber T.June = 6
toMonthNumber T.July = 7
toMonthNumber T.August = 8
toMonthNumber T.September = 9
toMonthNumber T.October = 10
toMonthNumber T.November = 11
toMonthNumber T.December = 12

createDayWithInt::Int -> Day
createDayWithInt x = Day x []

createDay:: IO Day 
createDay = do
	c <- T.getClockTime
	cal <- T.toCalendarTime c
	return (Day (T.ctDay cal) [])	

addYearToCalendar:: Year -> Calendar -> Calendar
addYearToCalendar (Year i ms) (Calendar xs) = Calendar ys
	where
		us = filter (\s -> i /= getYearNumber s) xs
		ys = (Year i ms):us 

getYearNumber:: Year -> Int 
getYearNumber (Year i _ ) = i

createEvent::Title -> Event
createEvent title = Event title 

createEventDue:: Title -> Event
createEventDue title = Event (title ++ " is due today")

getDayFromMonth::Int -> Month -> Maybe Day 
getDayFromMonth x (Month i []) 			  = Just $ Day x []
getDayFromMonth x (Month i ((Day z es):ys)) | x==z  = Just $ Day z es
				  | otherwise = getDayFromMonth x (Month i ys)

getMonthFromYear::Int -> Year -> Maybe Month
getMonthFromYear _ (Year i []) 				= Nothing
getMonthFromYear x (Year i ((Month z ds):ys)) | x==z	= Just $ Month z ds
					      |otherwise= getMonthFromYear x (Year i ys)

getYearFromCalendar:: Int -> Calendar -> Maybe Year
getYearFromCalendar _ (Calendar []) 				= Nothing
getYearFromCalendar x (Calendar ((Year z ms):ys)) | x==z 	= Just $ Year z ms
						  | otherwise 	= getYearFromCalendar x (Calendar ys)

getDayFromMonthFromYear::Int -> Int -> Maybe Year -> Maybe Day
getDayFromMonthFromYear m d Nothing = Nothing
getDayFromMonthFromYear m d (Just x) = getMonthFromYear m x >>= getDayFromMonth d 

addEventToDay:: Event -> Day -> Maybe Day
addEventToDay e (Day x ys) = Just $ Day x (e:ys)

addEventToMonth::Event -> Int -> Month -> Maybe Month 
addEventToMonth e x month@(Month n ds) = case getDayFromMonth x month of
	Nothing -> Just $ Month n (d:ds)
		where 
			Just d = addEventToDay e $ createDayWithInt x
	Just mD -> Just $ Month n ds2 
		where
			r = filter (mD /= ) ds
			Just w = addEventToDay e mD
			ds2 = w:r

addEventToYear:: Event -> Int -> Int -> Year -> Maybe Year
addEventToYear e m d year@(Year y ms) = case getMonthFromYear m year of
	Nothing -> Just $ Year y (newM:ms)
		where
			Just newM = addEventToMonth e d $ createMonthWithInt m 
	Just oldM -> Just $ Year y ms2
		where
			r = filter (oldM /= ) ms
			Just newM = addEventToMonth e d oldM
			ms2 = newM:r 

removeEventFromDay::Event -> Day -> Day
removeEventFromDay e (Day i es) = Day i es2
	where
		es2 = filter (e/=) es

removeEventFromMonth:: Event -> Int -> Month -> Month
removeEventFromMonth e i m  = case getDayFromMonth i m of
	Nothing -> m
	Just d 	-> Month n ds
		where
		Month n ds2 = m
		r  = filter (d/=) ds2
		ds = (removeEventFromDay e d):r

removeEventFromYear::Event -> Int -> Int -> Year -> Year
removeEventFromYear e m d year = case getMonthFromYear m year of
	Nothing -> year 
	Just mm -> Year i newM
		where
		Year i oldM = year 
		r  = filter (mm/=) oldM
		newM = (removeEventFromMonth e d mm):r



showEventsFromDay:: Maybe Day -> [String]
showEventsFromDay Nothing = ["\tYou have now events on this day."]
showEventsFromDay (Just (Day n es)) = do
	let x = "Your Events for the " ++ show n++".\n"
	let y =   map (addNewLine . addTabs . show) es
	x:y

addTabs:: String -> String
addTabs x = "\t" ++ x

addNewLine:: String -> String
addNewLine x = x ++ "\n"

showEventsFromMonth:: Maybe Month -> [String]
showEventsFromMonth Nothing = ["\tYou have in this month.\n"]
showEventsFromMonth (Just (Month i ds)) = do
	let y = "Month Number " ++ show i++".\n"
	let x = map (showEventsFromDay . Just ) ds
	let w = concat x
	let e = y:w
	map addTabs e	

showEventsFromDayInMonth::Int -> Maybe Month -> [String]
showEventsFromDayInMonth _ Nothing = ["\tYou have no events on this day.\n"]
showEventsFromDayInMonth i (Just m) = case getDayFromMonth i m of
	Nothing -> ["\tNothing to do today!\n"]
	Just d -> showEventsFromDay (Just d)

showEventsFromMonthInYear:: Int -> Maybe Year -> [String]
showEventsFromMonthInYear _ Nothing = ["\tYou have no events in this year\n"]
showEventsFromMonthInYear x (Just year) = case getMonthFromYear x year of 
	Nothing -> ["\tNo Events for this Month\n"]
	Just m -> map addTabs e
		where
		e = showEventsFromMonth (Just m)
	


getMyCalendar:: IO Calendar
getMyCalendar = do
	home <- getHomeDirectory
	let file = home ++ "/.calendar"
	contents <- readFile file
	return $ read contents

putMyCalendar::Calendar -> IO ()
putMyCalendar calendar = do
	home <- getHomeDirectory
	let file = home ++ "/.calendar"
	writeFile file $ show calendar


