let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {};
  ghc = pkgs.haskell.packages.ghc98;
in
ghc.callCabal2nix "rash" sources.rash {}
