# Immutaball

Neverball rewrite in Haskell.

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
