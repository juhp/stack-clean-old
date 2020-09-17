# stack-clean-old

A small tool to clean away older Haskell stack snapshot builds,
to recover diskspace.

## Usage
```
stack-clean-old [project|snapshots|ghc] [size|list|remove-version] [GHCVER]
```
These commands act respectively on:

- the current local project (`.stack-work/`)
- the user's stack snapshot builds (`~/.stack/snapshots/`)
- installed stack ghc compilers  (`~/.stack/programs/`).

`size`:
    prints the total size of stack builds for the project or snapshots.
    (Note `size` does not take a GHCVER argument.)

`list`:
    shows the total size number of snapshots per ghc version
    (The GHCVER argument is optional.)

`remove-version`:
    removes all snapshots for a version of ghc
    (A GHCVER argument is required.)

NB: if you remove all snapshot builds for a version of ghc, then you would have to rebuild again for any projects still using that snapshot version, so removal should be used cautiously, but it can recover a lot of diskspace.

### Purging older stack project builds
```
stack-clean-old project remove-older
```
This command removes older stack builds from `.stack-work/install/`.
By default it keeps 5 newest builds per ghc version.

Preservation/deletion is done per ghc version.

## Installation

Run `stack install` or `cabal install`

## Contributing
BSD license

Project: https://github.com/juhp/stack-clean-old

## Warning
Use at your own risk.

The author takes no responsibility for any loss caused by using this tool.
But bug reports, suggestions, and improvements are welcome.
