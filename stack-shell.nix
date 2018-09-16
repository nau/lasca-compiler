with (import <nixpkgs> {});
{ghc ? haskell.compiler.ghc822}:
let 
    lascart = pkgs.callPackage ./lascart.nix {};
in haskell.lib.buildStackProject {
  name = "lasca";
  buildInputs = [ lascart boehmgc pcre2 ];
  src = ./.;
}