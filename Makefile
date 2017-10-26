PANDOC = pandoc
IFORMAT = markdown
FLAGS = --standalone --toc --highlight-style pygments

#CC = gcc
CC = clang-5.0 -Wno-nullability-completeness -Wno-expansion-to-defined
CC_INCLUDE = -I/usr/local/include -I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/
LLC = llc-5.0

OPTS = -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d

#TEST_RTS = +RTS -sstderr
TEST_RTS =

bench:
	time lasca -O2 -e src/main/lasca/gen.lasca

rts:
	$(CC) -S -emit-llvm -g -O2 $(CC_INCLUDE) src/main/c/*.c*
	$(CC) -shared -fPIC -g -O3 $(CC_INCLUDE)  -L/usr/local/lib -lgc -lstdc++ src/main/c/*.c* -o liblascart.so

rtsDebug:
	$(CC) -shared -fPIC -g -O0 -I/usr/local/include -L/usr/local/lib -lgc -lstdc++ src/main/c/*.c* -o liblascart.so

rusts:
	cd src/main/rust && cargo build && cp target/debug/liblascarts.dylib ../../../
    
lasca: rts
	stack install

test:
	stack test

examples:
	lasca -O2 -e --mode static  src/main/lasca/array.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic src/main/lasca/array.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  src/main/lasca/data.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic src/main/lasca/dynamic.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  src/main/lasca/factorial.lasca $(TEST_RTS) -- 15
	lasca -O2 -e --mode dynamic src/main/lasca/factorial.lasca $(TEST_RTS) -- 15
	lasca -O2 -e --mode dynamic src/main/lasca/hello.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  src/main/lasca/hello.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  src/main/lasca/lambda.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic src/main/lasca/lambda.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  src/main/lasca/list.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic src/main/lasca/list.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  src/main/lasca/nbody.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode dynamic src/main/lasca/nbody.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode static  src/main/lasca/nbody2.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode dynamic  src/main/lasca/nbody2.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode static  src/main/lasca/nbody3.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode dynamic  src/main/lasca/nbody3.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode static  src/main/lasca/option.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic src/main/lasca/option.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  src/main/lasca/ski.lasca $(TEST_RTS)
#	lasca -O2 -e src/main/lasca/typed.lasca $(TEST_RTS)

install_and_examples: lasca examples

release: install_and_examples
	rm -rf dist
	mkdir -p dist/{src,bash_completion}
	cp .stack-work/dist/x86_64-osx/Cabal-2.0.0.2/build/lasca/lasca dist
	cp liblascart.so dist
#	find src/main/lasca -name *.lasca -exec cp \{} dist/src \;
	cp src/main/lasca/Prelude.lasca dist/src
#	lasca --bash-completion-script lasca > dist/$(brew --prefix)/etc/bash_completion.d/lasca
	lasca --bash-completion-script lasca > dist/bash_completion/lasca
	(cd dist; tar -czf ../lasca-0.0.1.tar.gz .)
	SUM="$(shell shasum -a 256 lasca-0.0.1.tar.gz | awk '{ print $$1 }')"
#	echo $(SUM)
#	sed -E -e 's/sha256 "[a-zA-Z0-9]+"/sha256 $(SUM)/' ../homebrew-lasca/lasca-compiler.rb


designpdf:
	rst2pdf -b 1 docs/Lasca\ Design.rst

.PHONY: clean
