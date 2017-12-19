let
  pkgs = import <nixpkgs> { };
  lasca = pkgs.callPackage ./default.nix {stdenv = pkgs.llvmPackages_5.stdenv;};
in lasca
