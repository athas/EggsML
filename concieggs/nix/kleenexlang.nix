let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {};
  ghc = pkgs.haskell.packages.ghc98;
in
ghc.callCabal2nix "kleenexlang" sources.kleenexlang
  {
    kmc-regexps-syntax =
      pkgs.haskell.lib.overrideCabal
        (ghc.callCabal2nix "regexps-syntax" sources.regexps-syntax { })
          (_: {
            jailbreak = true;
          });
  }
