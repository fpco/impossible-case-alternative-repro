# impossible-case-alternative-repro

This reproduces an `Impossible case alternative` error with GHC 7.10 and 7.8:

```
ghc --make -O -Wall Main.hs && ./Main
```

Output:

```
Main: Impossible case alternative
```

This does not happen when you turn optimisations off with `-O0`.

It needs a few dependencies, mainly `aeson`; for these, see `impossible-case-alternative-repro.cabal`.
