# Immutaball

Immutaball is a rewrite of Neverball in Haskell.  It is useful for gaming and
studying a purely functional FRP application.  It does not use IO except for
interfacing with external dependencies and ‘base’/‘haskell2010’ IO primitives
like writing to a file (including concurrent evaluation).

## Build notes

### wires 0.2.1

As of 2024-09-27, the latest wires dependency, v0.2.1, is a little out of date.
Simply building a new local version e.g. v0.2.1.0.1, with updated dependency
upper bounds with a new semialign dependency with the following changes is sufficient to build wires.

Thus this package depends on wires > v0.2.1.

You may build it with ‘cabal install --lib --package-env=./package-env.txt’,
looking for the package-db path inside ./package-env.txt afterwards, and then
building immutaball with e.g.
‘cabal build --package-db=~/.local/state/cabal/store/ghc-9.4.2/package.db’

(You may also consider manually installing all deps with ‘cabal install --lib
dep’ before building old deps like wires, to help it pick recent versions of
dependencies, without later failing to choose newer dependencies because wires
was earlier built with older dependencies and must be re-built.)

#### Semialign change

In Control/Wires/Internal.hs, replace

```
instance Align Event where
    nil = NotNow
```

with

```
instance Align Event where
    nil = NotNow
instance Semialign Event where
```

#### Utils change

In Control/Wires/Utils.hs, replace

```
import Data.These
```

with

```
import Data.These.Combinators
```
