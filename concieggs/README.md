# concieggs

concieggs afhænger af:

  + sic
  + gawk
  + https://github.com/soimort/translate-shell
  + https://github.com/kurtmckee/feedparser
  + en masse Perl-moduler
  + en bunke Haskell-pakker
  + en række OCaml-pakker
  + alt muligt mere

Se `shell.nix` for et overblik.

De eneste scripts der må være i rodmappen er dem der bliver brugt direkte af
`concieggsd`.

## Perl-moduler

Prøv at køre denne kommando, og så smide dem igennem `cpan -T -i <moduler>`:

```
grep -hPr '^\s*use (\S*).*;' . | cut -d' ' -f2 | cut -d';' -f1 | sort | uniq | grep -v 'EggsML' | grep -P '^[A-Z]' --color=never | tr '\n' ' '
```

Vi afhænger vist af nogle Perl-moduler der ikke er i `shell.nix` (men ikke nogen vigtige?).

## Haskell-pakker

Prøv at køre denne kommando:

```
cabal install --lib mtl containers shell-escape MonadRandom random-fu random-extras xml ieee754 QuickCheck
```

Forresten: Vi har egentlig også ting der afhænger af her Haskell-pakker, men de er svære at installere on OpenBSD: `MissingH http-client http-client-tls aeson`

## OCaml-pakker

Prøv at køre denne kommando:

```
opam install ocamlfind ppx_let core async async_ssl cohttp-async yojson
```
