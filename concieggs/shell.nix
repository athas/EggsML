# Use this file with nix-shell or similar tools; see https://nixos.org/
with import <nixpkgs> {};

mkShell {
  buildInputs = [
    sic
    gawk
    (python27.withPackages (ps: with ps; [
      numpy
    ]))
    (python3.withPackages (ps: with ps; [
      feedparser yfinance lxml
    ]))
    (perl532.withPackages (ps: with ps; [
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
    nim
    fsharp
    clang
    gnuapl
    ocaml
    dune_2
    ocamlPackages.findlib
    ocamlPackages.ppx_let
    ocamlPackages.core
    ocamlPackages.async
    ocamlPackages.async_ssl
    ocamlPackages.cohttp-async
    ocamlPackages.yojson
    gfortran
    (haskellPackages.ghcWithPackages (ps: with ps; [

    ]))
  ];
}
