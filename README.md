# impossible-case-alternative-repro - now also with segfault!

This reproduces an `Impossible case alternative` error with GHC 7.10 and 7.8:

```
ghc --make -O -Wall Main.hs && ./Main
```

Output:

```
"before eval"
bash: line 1: 19100 Segmentation fault      (core dumped) ./Main
```

This does not happen when you turn optimisations off with `-O0`.

It needs a few dependencies, mainly `aeson`; for these, see `impossible-case-alternative-repro.cabal`.
