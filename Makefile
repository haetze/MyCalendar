INPUTS := $(wildcard src/*.hs)
OUTPUTS := $(patsubst src/%,out/%, $(INPUTS))

all: $(OUTPUTS)
	@echo > Helo

out/%: in/% out
	cp $< $@

out:
	mkdir -p out

