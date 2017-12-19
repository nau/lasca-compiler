{ mkDerivation, base, bytestring, containers, directory, filepath
, haskeline, lascart, lens, llvm-hs, llvm-hs-pure, megaparsec, mtl
, murmur-hash, optparse-applicative, prettyprinter, process
, scientific, stdenv, tasty, tasty-hunit, tasty-quickcheck
, tasty-smallcheck, text, utf8-string
}:
mkDerivation {
  pname = "lasca";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory filepath haskeline lens
    llvm-hs llvm-hs-pure megaparsec mtl murmur-hash
    optparse-applicative prettyprinter process scientific text
    utf8-string
  ];
  librarySystemDepends = [ lascart ];
  executableHaskellDepends = [ base ];
  executableSystemDepends = [ lascart ];
  testHaskellDepends = [
    base megaparsec tasty tasty-hunit tasty-quickcheck tasty-smallcheck
    text utf8-string
  ];
  license = stdenv.lib.licenses.mit;
}
