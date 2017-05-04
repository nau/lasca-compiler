PANDOC = pandoc
IFORMAT = markdown
FLAGS = --standalone --toc --highlight-style pygments

CC = gcc
#CC = clang-4.0
LLC = llc-4.0

OPTS = -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d

#TEST_RTS = +RTS -sstderr
TEST_RTS =

bench:
	time lasca -O2 -e src/main/lasca/gen.lasca +RTS -sstderr

rts:
	$(CC) -shared -fPIC -g -O3 -I/usr/local/include -L/usr/local/lib -lgc -lstdc++ src/main/c/*.c* -o liblascart.so

rtsDebug:
	$(CC) -shared -fPIC -g -O0 -I/usr/local/include -L/usr/local/lib -lgc -lstdc++ src/main/c/*.c* -o liblascart.so

lasca:
	stack install

test:
	stack test

examples:
	lasca -O2 -e src/main/lasca/array.lasca $(TEST_RTS)
	lasca -O2 -e src/main/lasca/data.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic src/main/lasca/dynamic.lasca $(TEST_RTS)
	lasca -O2 -e src/main/lasca/factorial.lasca $(TEST_RTS) -- 15
	lasca -O2 -e src/main/lasca/hello.lasca $(TEST_RTS)
	lasca -O2 -e src/main/lasca/lambda.lasca $(TEST_RTS)
	lasca -O2 -e src/main/lasca/list.lasca $(TEST_RTS)
	lasca -O2 -e src/main/lasca/nbody.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e src/main/lasca/nbody2.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e src/main/lasca/ski.lasca $(TEST_RTS)
	lasca -O2 -e src/main/lasca/typed.lasca $(TEST_RTS)

install_and_examples: lasca examples


designpdf:
	rst2pdf -b 1 docs/Lasca\ Design.rst

.PHONY: clean
