let
  pkgs = import (builtins.fetchGit {
      name = "nixpkgs";
  url = "https://github.com/nixos/nixpkgs.git";
  rev = "6ec64973bc3a48b0c54d11c782e8b88b550a8eab";
  ref = "release-18.09";}) {};
  lascart = pkgs.callPackage ./lascart.nix {};
in
  pkgs.haskellPackages.callPackage ./lasca.nix { inherit lascart; }