# Prototype

Currently this project only implements basic parts of the game: minimal GUI,
basic physics, and a renderer.  Much of the game is currently unimplemented
(audio, goals, moving bodies, gameplay mechanics, a more efficient physics
implementation), but this project still serves as a useful example of a purely
functional FRP application.

Currently the physics does not apply BSP partitioning but brute force checks
collisions with every body in the level each frame.  Larger levels can't handle
this, but many of the levels in the first three level sets are playable.

# Immutaball

Immutaball is a rewrite of Neverball in Haskell.  It is useful for gaming and
studying a purely functional FRP application.  It does not use IO except for
interfacing with external dependencies and ‘base’/‘haskell2010’ IO primitives
like writing to a file (including concurrent evaluation).

# Screenshots and demo

![screenshot: readme-screenshot0-v0.1.0.1.png](doc/screenshots/readme-screenshot0-v0.1.0.1.png)

Demo video: <https://byronjohnson.net/immutaball/immutaball-v0.1.0.1-demo.html>

## Usage example

```
(cd -- "${HOME}/git/neverball" && make -j7)  # build neverball
cabal run --package-db="${HOME}/.local/state/cabal/store/ghc-9.13.20240927/package.db" immutaball -- -d ~/git/neverball/data
```
