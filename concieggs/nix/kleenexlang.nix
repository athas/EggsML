let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {};
in
pkgs.haskell.lib.buildStackProject {
  name = "kleenexlang";
  src = pkgs.stdenv.mkDerivation {
    name = "kleenexlang-with-regexps-syntax";
    src = [ sources.kleenexlang sources.regexps-syntax ];
    phases = "installPhase";
    installPhase = ''
      mkdir -p "$out"
      cp -r ${sources.regexps-syntax} "$out/regexps-syntax"
      cp -r ${sources.kleenexlang}/* "$out/"
      sed -i 's/lts-7.18/lts-20.25/' $out/stack.yaml
    '';
  };
}
