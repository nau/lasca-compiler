let
  pkgs = import <nixpkgs> { };
  lascart = pkgs.callPackage ./default.nix {stdenv = pkgs.llvmPackages_5.stdenv;};
in
  pkgs.haskellPackages.callPackage ./lasca.nix { inherit lascart; }
