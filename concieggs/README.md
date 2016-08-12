# concieggs

concieggs afhænger af:

  + sic
  + gawk
  + https://github.com/soimort/translate-shell
  + https://github.com/kurtmckee/feedparser
  + en masse perl-moduler(\*)
  + noget mere

De eneste scripts der må være i rodmappen er dem der bliver brugt direkte af
concieggsd.

(\*): Prøv at køre denne kommando, og så smide dem igennem `cpan -i <moduler>`:

```
grep -hPr '^\s*use (\S*).*;' . | cut -d' ' -f2 | cut -d';' -f1 | sort | uniq | grep -P '^[A-Z]' --color=never | tr '\n' ' '
```
