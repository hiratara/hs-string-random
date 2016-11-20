random-string
=============

## Description

This package is a Haskell port of [String_random.js library](https://github.com/cho45/String_random.js). There are implementations of [Perl](https://metacpan.org/pod/String::Random), [golang](https://github.com/Songmu/strrand), and [Java](https://github.com/moznion/java-random-string) as implementations of other languages.

## SINOPSIS

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Test.StringRandom

main = do
  ymd <- stringRandomIO "20\\d\\d-(1[0-2]|0[1-9])-(0[1-9]|1\\d|2[0-8])"
  print ymd -- "2048-12-08" etc.
```

## Author

Masahiro Honma (<hiratara@cpan.org>)
