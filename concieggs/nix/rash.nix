let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {};
  ghc = (import ./ghc.nix) pkgs;
in
ghc.callCabal2nix "rash" sources.rash {}
