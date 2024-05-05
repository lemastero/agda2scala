[![CI](https://github.com/lemastero/agda2scala/actions/workflows/haskell.yml/badge.svg?branch=main)](https://github.com/lemastero/agda2scala/actions?query=workflow%3A%22build%22+branch%3Amain)

# agda2scala


## Examples
are in [./examples](examples)

## Working with source code

* Starting continuous compilation loop

```sh
ghcid
```

* Build

```sh
cabal build all
```

* Run

```sh
cabal run -- agda2scala --help
cabal run -- agda2scala ./examples/adts.agda
```

* Run tests

```sh
cabal test all
```

## Resources
* Documentation for [Agda as Haskell library on Hackage](https://hackage.haskell.org/package/Agda) including
  * docs for [Agda.Compiler.Backend](https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-Backend.html)
  * build-in [JS backend](https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-JS-Compiler.html)
  * build-in [Haskell backend](https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-MAlonzo-Compiler.html)
* external project with Agda backends 
  * [omelkonian/agda-minimal-backend](https://github.com/omelkonian/agda-minimal-backend) 
  * [jespercockx/agda2scheme](https://github.com/jespercockx/agda2scheme)
  * [omelkonian/agda2train](https://github.com/omelkonian/agda2train)
  * [agda/agda2hs](https://github.com/agda/agda2hs), ([publication](https://iohk.io/en/research/library/papers/reasonable-agda-is-correct-haskell-writing-verified-haskell-using-agda2hs/))
  * [HectorPeeters/agda2rust](https://github.com/HectorPeeters/agda2rust), ([publication](https://repository.tudelft.nl/islandora/object/uuid:39bff395-1bd6-4905-8554-cef0cd5e7d3e))
  * [lemastero/agda2rust](https://github.com/lemastero/agda2rust)
