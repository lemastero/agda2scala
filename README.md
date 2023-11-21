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
* docs for [Agda package](https://hackage.haskell.org/package/Agda) on Hackage
  * [Agda.Compiler.Backend](https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-Backend.html)
  * [JS compiler](https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-JS-Compiler.html)
  * [GHC compiler](https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-MAlonzo-Compiler.html)
* [agda2scheme](https://github.com/jespercockx/agda2scheme)
* [agda2hs](https://github.com/agda/agda2hs)
