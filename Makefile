PANDOC = pandoc
IFORMAT = markdown
FLAGS = --standalone --toc --highlight-style pygments

CC = gcc

OPTS = -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d

all: compile_example
	node example1.nl.js
	./compiled

%.ll: newlang
	stack exec nemish

newlang:
	#ghc $(OPTS) --make src/*.hs -o newlang
	stack build

compile_example: %.ll
	$(CC) -fPIC *.ll -o compiled


.PHONY: clean

clean:
	rm -f compiled *.ll *.js