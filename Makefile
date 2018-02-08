PANDOC = pandoc
IFORMAT = markdown
FLAGS = --standalone --toc --highlight-style pygments

#TEST_RTS = +RTS -sstderr
TEST_RTS =

build: rts 
	stack build

install: build
	stack install && stack test

bench:
	time lasca -O2 -e examples/gen.lasca

rts:
	mkdir -p build && cd build && cmake .. && make && sudo make install

rusts:
	cd rts/rust && cargo build && cp target/debug/liblascarts.dylib ../../../
    
lasca: rts
	stack install

test:
	stack test

examples:
	lasca -O2 -e --mode static  examples/array.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic examples/array.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  examples/data.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic examples/data.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic examples/dynamic.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  examples/factorial.lasca $(TEST_RTS) -- 15
	lasca -O2 -e --mode dynamic examples/factorial.lasca $(TEST_RTS) -- 15
	lasca -O2 -e --mode dynamic examples/hello.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  examples/hello.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  examples/lambda.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic examples/lambda.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  examples/list.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic examples/list.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  examples/nbody.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode dynamic examples/nbody.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode static  examples/nbody2.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode dynamic  examples/nbody2.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode static  examples/nbody3.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode dynamic  examples/nbody3.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode static  examples/binarytrees.lasca $(TEST_RTS) -- 10
	lasca -O2 -e --mode dynamic  examples/binarytrees.lasca $(TEST_RTS) -- 10
	lasca -O2 -e --mode static  examples/Option.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic examples/Option.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  examples/Map.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic examples/Map.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  examples/ski.lasca $(TEST_RTS)
#	lasca -O2 -e examples/typed.lasca $(TEST_RTS)

install_and_examples: lasca examples

release: install_and_examples
	rm -rf dist
	mkdir -p dist/{src,bash_completion}
	cp .stack-work/dist/x86_64-osx/Cabal-2.0.0.2/build/lasca/lasca dist
	cp liblascart.so dist
#	find examples -name *.lasca -exec cp \{} dist/src \;
	cp examples/Prelude.lasca dist/src
#	lasca --bash-completion-script lasca > dist/$(brew --prefix)/etc/bash_completion.d/lasca
	lasca --bash-completion-script lasca > dist/bash_completion/lasca
	(cd dist; tar -czf ../lasca-0.0.1.tar.gz .)
	SUM="$(shell shasum -a 256 lasca-0.0.1.tar.gz | awk '{ print $$1 }')"
#	echo $(SUM)
#	sed -E -e 's/sha256 "[a-zA-Z0-9]+"/sha256 $(SUM)/' ../homebrew-lasca/lasca-compiler.rb


designpdf:
	rst2pdf -b 1 docs/Lasca\ Design.rst

.PHONY: clean examples rts install
