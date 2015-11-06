# MyCalendar
MyCalendar is a small command line tool written in Haskell. It works like an event scheduler in the sense that 
it allows to add events to days only instead of days and times. It now also has the option to add a time to an event.
In that case the events are organized in a different list. This is list is sorted by time (earlier in the day is 
higher). 
I added a weekly schedule. The weekly schedule can be used to keep track of events that we occure on a weekly basis. 
The week is printed in a nice table view.


##Updates

###Update 1
1.Time option added
2.Output is now sorted 
3.Files are not longer corrupted in case of an exception

###Update 2
1. Second case of file corruption fixed

###Update 3
1. Schedule handling 
  1. Create complete schedule
  2. Print complete schedule
  3. Print "work day" (from 8:00am to 8:00pm)
2. Schedule safed in ~/.week
