let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
with pkgs;
mkShell {
  buildInputs = [
    sic
    gawk
    (python27.withPackages (ps: with ps; [
      numpy
    ]))
    (python3.withPackages (ps: with ps; [
      feedparser yfinance lxml opencc requests beautifulsoup4
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
    cargo
    nim
    fsharp
    clang
    gnuapl
    futhark
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
    sbcl
    clisp
    myrddin
    (haskellPackages.ghcWithPackages (ps: with ps; [
      mtl
      containers
      shell-escape
      MonadRandom
      random-fu
      MissingH
      http-client
      http-client-tls
      aeson
      xml
      ieee754
      QuickCheck
    ]))
    rust-script
    (import ./nix/kleenexlang.nix)
    tcl
    ruby
    nodejs-16_x
  ];

  shellHook = ''
    export NODE_PATH=${(pkgs.callPackage ./nix/node/default.nix {}).nodeDependencies}/lib/node_modules:$NODE_PATH
  '';
}
