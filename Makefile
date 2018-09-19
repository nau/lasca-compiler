PANDOC = pandoc
IFORMAT = markdown
FLAGS = --standalone --toc --highlight-style pygments

#TEST_RTS = +RTS -sstderr
TEST_RTS =

build: rts
	stack build --extra-lib-dirs=build/rts

fast: rts
	stack build --fast -j 8 --extra-lib-dirs=build/rts

install: build
	stack install --extra-lib-dirs=build/rts && stack test --extra-lib-dirs=build/rts

fastinstall: fast
	stack install --fast --extra-lib-dirs=build/rts

bench:
	time lasca -O2 -e examples/gen.lasca

rts:
	mkdir -p build && cd build && cmake -DCMAKE_BUILD_TYPE=Debug .. && make && cp rts/liblascart* $(LASCAPATH)

relink: rts
	rm -rf .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Lasca/lasca
	rm -rf .stack-work/install
	stack build --fast -j 8 --copy-bins --extra-lib-dirs=build/rts

rusts:
	cd rts/rust && cargo build && cp target/debug/liblascarts.dylib ../../../

test:
	stack test --extra-lib-dirs=build/rts

fasttest:
	stack test -j 8 --fast --extra-lib-dirs=build/rts

examples:
	lasca -O2 -e --mode static  libs/base/Array.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic libs/base/Array.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  libs/base/List.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic libs/base/List.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  libs/base/Option.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic libs/base/Option.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  libs/base/String.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic libs/base/String.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  examples/Map.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic examples/Map.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  examples/Data.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic examples/Data.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic examples/dynamic.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  examples/factorial.lasca $(TEST_RTS) -- 15
	lasca -O2 -e --mode dynamic examples/factorial.lasca $(TEST_RTS) -- 15
	lasca -O2 -e --mode dynamic examples/hello.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  examples/hello.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  examples/lambda.lasca $(TEST_RTS)
	lasca -O2 -e --mode dynamic examples/lambda.lasca $(TEST_RTS)
	lasca -O2 -e --mode static  examples/nbody.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode dynamic examples/nbody.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode static  examples/nbody2.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode dynamic examples/nbody2.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode static  examples/nbody3.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode dynamic examples/nbody3.lasca $(TEST_RTS) -- 50000
	lasca -O2 -e --mode static  examples/binarytrees.lasca $(TEST_RTS) -- 10
	lasca -O2 -e --mode dynamic examples/binarytrees.lasca $(TEST_RTS) -- 10
	lasca -O2 -e --mode static  examples/ski.lasca $(TEST_RTS)

perf:
	stack install --profile -j 8 --extra-lib-dirs=build/rts
	time lasca examples/Map.lasca +RTS -sstderr -N4 -p -hc
	hp2ps -c lasca.hp
	ghc-prof-flamegraph lasca.prof

release: lasca test
	rm -rf dist
	mkdir -p dist/{src,bash_completion}
	cp .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/lasca/lasca dist
	cp liblascart.a dist
	cp libs/base/*.lasca dist/src
#	lasca --bash-completion-script lasca > dist/$(brew --prefix)/etc/bash_completion.d/lasca
	lasca --bash-completion-script lasca > dist/bash_completion/lasca
	(cd dist; tar -czf ../lasca-0.1.0.tar.gz .)
	SUM="$(shell shasum -a 256 lasca-0.1.0.tar.gz | awk '{ print $$1 }')"
#	echo $(SUM)
#	sed -E -e 's/sha256 "[a-zA-Z0-9]+"/sha256 $(SUM)/' ../homebrew-lasca/lasca-compiler.rb


designpdf:
	rst2pdf -b 1 docs/Lasca\ Design.rst

.PHONY: clean examples rts install
