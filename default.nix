{stdenv, libffi, boehmgc}:
    stdenv.mkDerivation {
        name = "liblascart-0.1";
        src = ./.;
        buildInputs = [libffi boehmgc];
    } 
