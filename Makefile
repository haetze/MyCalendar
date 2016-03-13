#Makefile for MyCalendar
#

INPUTS := $(wildcard src/*.hs)
OUTPUTS := $(patsubst src/%.hs,src/%.o, $(INPUTS))

all: src/main.hs $(OUTPUTS)
	ghc -isrc/ src/main.hs
	cp src/main bin/myCal
	sudo cp bin/myCal /usr/local/bin/myCal

src/%.o: src/%.hs 
	ghc -isrc/  $< 


