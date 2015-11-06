#! /bin/sh
#
# build.sh
# Copyright (C) 2015 haetze <haetze@home>
#
# Distributed under terms of the MIT license.
#

mkdir -p bin 
ghc ./src/Calender.hs 
ghc ./src/Schedule.hs 
ghc -isrc/ ./src/main.hs -o ./bin/main
