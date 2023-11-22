[![CI](https://github.com/lemastero/agda2scala/actions/workflows/haskell.yml/badge.svg?branch=main)](https://github.com/lemastero/agda2scala/actions?query=workflow%3A%22build%22+branch%3Amain)

# agda2scala

## Working with source code

* Update cabal libraries definitions

```sh
cabal update
```

* Build

```sh
cabal build all
```

* Run

```sh
cabal run -- agda2scala --help
cabal run -- agda2scala ./test/Hello.agda
```

* Run tests

```sh
cabal test all
```

## Resources
* [Agda package](https://hackage.haskell.org/package/Agda) on Hackage with
  * docs for [Agda.Compiler.Backend](https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-Backend.html)
  * build-in [JS backend](https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-JS-Compiler.html)
  * build-in [Haskell backend](https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-MAlonzo-Compiler.html)
* [agda2scheme](https://github.com/jespercockx/agda2scheme)
* [agda2hs](https://github.com/agda/agda2hs), ([publication](https://iohk.io/en/research/library/papers/reasonable-agda-is-correct-haskell-writing-verified-haskell-using-agda2hs/))
* [agda2rust](https://github.com/HectorPeeters/agda2rust), ([publication](https://repository.tudelft.nl/islandora/object/uuid:39bff395-1bd6-4905-8554-cef0cd5e7d3e))
