#Makefile for MyCalendar
#

INPUTS := $(wildcard src/Modules/*.hs)
OUTPUTS := $(patsubst src/Modules/%.hs,src/Modules/%.o, $(INPUTS))

all: src/Main/main.hs $(OUTPUTS)
	ghc -isrc/Modules src/Main/main.hs
	cp src/Main/main bin/main

src/Modules/%.o: src/Modules/%.hs 
	ghc $< 


