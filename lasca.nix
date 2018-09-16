{ mkDerivation, base, bytestring, containers, directory, filepath
, boehmgc, Glob, haskeline, lascart, lens, llvm-hs, llvm-hs-pure
, megaparsec, mtl, multiset, murmur-hash, optparse-applicative
, pcre2, prettyprinter, process, scientific, shelly, stdenv, tasty
, tasty-golden, tasty-hunit, tasty-program, tasty-quickcheck
, tasty-smallcheck, text, utf8-string, libffi, ...
}@args:
mkDerivation {
  pname = "lasca";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory filepath haskeline lens
    llvm-hs llvm-hs-pure megaparsec mtl multiset murmur-hash
    optparse-applicative prettyprinter process scientific text
    utf8-string libffi
  ];
  executableHaskellDepends = [ base ];
  executableSystemDepends = [ boehmgc lascart pcre2 ];
  testHaskellDepends = [
    base bytestring filepath Glob megaparsec shelly tasty tasty-golden
    tasty-hunit tasty-program tasty-quickcheck tasty-smallcheck text
    utf8-string libffi
  ];
  testSystemDepends = [ lascart libffi ];
  dontStrip = true;
  doCheck = false;
  preCheck = ''
    export PATH="$PATH:dist/build/lasca"
    export LASCAPATH="$out/libs/base"
    echo "New PATH = $PATH"
    echo "LASCAPATH = $LASCAPATH"
    mkdir -p "$out/libs/base"
    cp -r $src/libs/base/* "$out/libs/base"
    cp "${stdenv.lib.getLib lascart}/lib/liblascartStatic.a" "$out/libs/base"
    # ld -v 2
    # lasca --verbose -O2 examples/hello.lasca
  '';
  license = stdenv.lib.licenses.bsd3;
}
