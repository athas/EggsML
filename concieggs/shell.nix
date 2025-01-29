let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
with pkgs;
mkShell {
  buildInputs = [
    sic
    gawk
    (python3.withPackages (ps: with ps; [
      feedparser yfinance lxml opencc requests beautifulsoup4 python-dateutil
    ]))
    (perl540.withPackages (ps: with ps; [
      DateTime DateTimeFormatISO8601 Encode Env FileFindRule FilePath
      FileReadBackwards GitRepository IOAll IPCRun IPCSystemSimple JSON
      LinguaTranslit ListMoreUtils ListUtilsBy LWP MojoDOM58 StringUtil
      SubInstall TextAspell TextRoman TextUnaccent TimePiece TryTiny URI
      YAMLTiny
    ]))
    bash
    bc
    jq
    curl
    cowsay
    gcc
    binutils
    gnumake
    go
    gnu-cobol
    mosml
    fpc
    jdk
    rustc
    cargo
    nim
    fsharp
    clang
    gnuapl
    futhark
    ocaml
    dune_3
    ocamlPackages.findlib
    ocamlPackages.ppx_let
    ocamlPackages.core
    ocamlPackages.cohttp-lwt-unix
    ocamlPackages.tls-lwt
    ocamlPackages.yojson
    gfortran
    sbcl
    clisp
    zig
    cabal-install
    ghc
    rust-script
    (import ./nix/kleenexlang.nix)
    (import ./nix/rash.nix)
    tcl
    ruby
    php
    emacs-nox
    futhark
    nodejs_23
    mapscii
  ];

  dontDetectOcamlConflicts = true;
}
