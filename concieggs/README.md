# concieggs

concieggs afhænger af:

  + sic
  + gawk
  + https://github.com/soimort/translate-shell
  + https://github.com/kurtmckee/feedparser
  + en masse Python-biblioteker
  + en masse Perl-moduler
  + en bunke Haskell-pakker
  + en række OCaml-biblioteker
  + alt muligt mere

Se `shell.nix` for et overblik.

De eneste scripts der må være i rodmappen er dem der bliver brugt
direkte af `concieggsd`.

## Perl-moduler

Prøv at køre denne kommando, og så smide dem igennem `cpanm -ni
<moduler>`:

```
grep -hPr '^\s*use (\S*).*;' . | cut -d' ' -f2 | cut -d';' -f1 | sort | uniq | grep -v 'EggsML' | grep -P '^[A-Z]' --color=never | tr '\n' ' '
```

Husk også at køre `cpanm -ni utf8::all local::lib
Git::Repository::Plugin::Blame WWW::Mechanize Sys::SigAction` oveni.

Vi afhænger vist af nogle Perl-moduler der ikke er i `shell.nix` (men
ikke nogen vigtige?).  Se også `eggspi/perleggs/cpanfile`.

## Haskell-pakker

Prøv at køre denne kommando:

```
cabal update && cabal install --disable-optimization --jobs=1 --lib mtl containers parsec shell-escape MonadRandom random random-fu xml ieee754 QuickCheck
```

Forresten: Vi har egentlig også ting der afhænger af her Haskell-pakker,
men de er svære at installere på OpenBSD: `http-client http-client-tls aeson`

## OCaml-pakker

Prøv at køre denne kommando:

```
opam install ocamlfind ppx_let core async async_ssl cohttp-async yojson
```

## Node-pakker

Prøv at køre det her:

```
npm install mapscii
```
