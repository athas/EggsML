# concieggs

concieggs afhænger af:

  + sic
  + gawk
  + https://github.com/soimort/translate-shell
  + https://github.com/kurtmckee/feedparser
  + en masse perl-moduler
  + en bunke Haskell-moduler
  + noget mere

De eneste scripts der må være i rodmappen er dem der bliver brugt direkte af
`concieggsd`.

## Perl-pakker

Prøv at køre denne kommando, og så smide dem igennem `cpan -T -i <moduler>`:

```
grep -hPr '^\s*use (\S*).*;' . | cut -d' ' -f2 | cut -d';' -f1 | sort | uniq | grep -v 'EggsML' | grep -P '^[A-Z]' --color=never | tr '\n' ' '
```

## Haskell-pakker

```
cabal install --lib mtl containers MonadRandom random-fu random-extras xml ieee754 QuickCheck
```

Forresten: Vi har egentlig også ting der afhænger af her Haskell-pakker, men de er svære at installere on OpenBSD: `MissingH http-client http-client-tls aeson`
