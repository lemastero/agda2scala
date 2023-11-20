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
