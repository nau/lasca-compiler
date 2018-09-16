{stdenv, cmake, libffi, boehmgc, pcre2, pkgconfig, zlib}:
stdenv.mkDerivation rec {
    name = "lascart-${version}";
    version = "0.1.1";
    src = ./rts;
    nativeBuildInputs = [ pkgconfig cmake ];
    buildInputs = [ pcre2 boehmgc zlib libffi ];
    dontDisableStatic = true;
    cmakeFlags = [
        "-DCMAKE_BUILD_TYPE=Debug"
    ];
    dontStrip = true;
    meta = with stdenv.lib; {
        platforms = platforms.linux ++ platforms.darwin;
        license = licenses.bsd3;
        homepage = http://lasca-lang.org;
        description = "Lasca Runtime System";
  };
}