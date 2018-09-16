let
  pkgs = import <nixpkgs> { };
  lascart = pkgs.callPackage ./lascart.nix {};
in
  pkgs.haskellPackages.callPackage ./lasca.nix { inherit lascart; }