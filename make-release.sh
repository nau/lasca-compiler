#!/usr/bin/env bash

set -eux

makeRelease() {
    local version=$1
    echo "Making release of version $version"

    rm -rf dist
	mkdir -p dist/{bin,src,lib,bash_completion}
	cp "$(stack path --dist-dir)/build/lasca/lasca" dist/bin

	LLVM_PATH="$(brew --cellar)/llvm-6.0/6.0.0/lib/llvm-6.0/lib"
	echo "${LLVM_PATH}"
	cp "${LLVM_PATH}/libLLVM.dylib"    dist/lib
	cp "${LLVM_PATH}/libc++.1.0.dylib" dist/lib
	install_name_tool -add_rpath @executable_path/../lib dist/bin/lasca
	install_name_tool -change "${LLVM_PATH}/libLLVM.dylib" @rpath/libLLVM.dylib dist/bin/lasca
	install_name_tool -change "${LLVM_PATH}/libc++.1.0.dylib" @rpath/libc++.1.0.dylib dist/bin/lasca

	chmod 0644 dist/lib/*
	install_name_tool -id @rpath/libLLVM.dylib dist/lib/libLLVM.dylib
	install_name_tool -id @rpath/libc++.1.0.dylib dist/lib/libc++.1.0.dylib
	cp build/rts/liblascartStatic.a dist/src
	cp libs/base/*.lasca dist/src
	lasca --bash-completion-script lasca > dist/bash_completion/lasca
	(cd dist; tar -czf "../lasca-${version}.tar.gz" .)
	shasum -a 256 "lasca-${version}.tar.gz"
#	sed -E -e 's/sha256 "[a-zA-Z0-9]+"/sha256 $(SUM)/' ../homebrew-lasca/lasca-compiler.rb
}

makeRelease $1