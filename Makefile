PANDOC = pandoc
IFORMAT = markdown
FLAGS = --standalone --toc --highlight-style pygments

CC = gcc
LLC = llc-3.5

OPTS = -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d

all: medium

medium.nl: lasca
	stack exec gencode 10000 > medium.nl

medium.ll: medium.nl
	time stack exec lasca -- medium.nl +RTS -ssterr > medium.ll

medium.s: medium.ll
	time $(LLC) -O2 medium.ll

medium: medium.s
	time $(CC) -fPIC medium.s -o medium

rts:
	$(CC) -shared -fPIC -O2 -I/usr/local/include -L/usr/local/lib -lgc -lstdc++ src/main/c/*.c* -o liblascart.so

lasca:
	stack build

compile_example: %.s
	$(CC) -fPIC *.s -o compiled

designpdf:
	rst2pdf -b 1 Lasca\ Design.rst

.PHONY: clean

clean:
	rm -f compiled *.ll *.js medium*