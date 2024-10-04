# Immutaball

Immutaball is a rewrite of Neverball in purely functional Haskell.  It is
useful for gaming and studying a purely functional FRP application.  It does
not use IO except for interfacing with external dependencies and
‘base’/‘haskell2010’ IO primitives like writing to a file (including concurrent
evaluation).

## Build notes

### wires 0.2.1

As of 2024-09-27, the latest wires dependency, v0.2.1, is a little out of date.
Simply building a new local version e.g. v0.2.1.0.1, with updated dependency
upper bounds with a new semialign dependency with the following changes is
sufficient to build wires.

Thus this package depends on wires > v0.2.1.

To ensure that wires has consistent dependencies with this package, I recommend
you copy the build-deps from this package and append them to your local wires
source package, and then manually install the local wires source package by
running ‘cabal install --lib’ inside your git clone (this adds to the user
cabal store and updates e.g. `~/.ghc/x86_64-linux-9.4.2/environments/default`),
with the changes mentioned, including increasing the upper bounds of ‘wires’'s
existing dependencies (see ‘New Dependencies’).  Otherwise when building this
package, local packages such as your locally install ‘wires’ would be fixed to
the dependencies chosen when you built it, which may be incompatible with this
package's dependencies.

Old build tips:

(You may build it with ‘cabal install --lib --package-env=./package-env.txt’,
looking for the package-db path inside ./package-env.txt afterwards, and then
building immutaball with e.g.
‘cabal build --package-db=~/.local/state/cabal/store/ghc-9.4.2/package.db’)

(You may also consider manually installing all deps with ‘cabal install --lib
dep’ before building old deps like wires, to help it pick recent versions of
dependencies, without later failing to choose newer dependencies because wires
was earlier built with older dependencies and must be re-built.  Alternatively,
comment out ‘wires’ from the .cabal file, run cabal build so it builds the
dependencies except for ‘wires’, and then uncomment ‘wires’.)

(Finally, adding a ‘text >= 2.1.1 && < 2.2’ version to ‘wires’ before installing
it with ‘cabal install --lib’ inside the git clone may help cabal build this
package.  Additionally, you can even copy this package's dependencies and append them to
wires' dependencies to ensure the dependencies are consistent.)

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

#### New dependencies

```
base >= 4.8 && < 5,
deepseq >= 1.4.0 && < 1.7,
mtl >= 2.0 && < 5.7,
profunctors >= 5.0 && < 5.7,
semigroupoids >= 5.0 && < 6.1,
these >= 0.7.0 && < 1.3,
semialign >= 1.3.1 && < 1.4
```
