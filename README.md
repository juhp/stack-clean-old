# stack-clean-old

A small tool to clean away older Haskell [stack](https://docs.haskellstack.org)
snapshot builds and ghc versions, to recover diskspace.

## Usage
```
stack-clean-old [project|snapshots|ghc] [size|list|remove-version|remove-earlier-minor] [GHCVER]
```
These commands act respectively on:

- the current local project (`.stack-work/install/`)
- the user's stack snapshot builds (`~/.stack/snapshots/`)
- installed stack ghc compilers  (`~/.stack/programs/`).

and the subcommands:

`size`:
    prints the total size of the above directory
    (`size` does not take a GHCVER argument).

`list`:
    shows the total size and number of snapshots per ghc version
    (the GHCVER argument is optional).

`remove-version`:
    removes all snapshots for the specified ghc version
    (the GHCVER argument is required).

`remove-earlier-minor`:
    removes for previous minor ghc versions.
    If GHCVER is given then only minor versions older than it are removed.

NB: If you remove all snapshot builds for a version of ghc, then you would have to rebuild again for any projects still using them, so removal should be used cautiously, but it can recover a lot of diskspace.

### Example usage
To remove project builds for ghc-8.4.4:
```
$ stack-clean-old project list
154M  8.4.4  (5 dirs)
154M  8.6.5  (5 dirs)
163M  8.8.4  (5 dirs)
$ stack-clean-old project remove-version 8.4.4
```

Remove all stack snapshot builds for ghc-8.0.1:
```
$ stack-clean-old snapshots list
489M  8.0.1  (8 dirs)
799M  8.0.2  (24 dirs)
$ stack-clean-old snapshots remove-earlier-minor 8.0.2
```

### Purging older stack project builds
```
stack-clean-old project remove-older
```
This command removes older stack builds from `.stack-work/install/`.
By default it keeps 5 newest builds per ghc version.

The preservation/deletion is calculated and done per ghc version.

NB: If you regularly build multiple branches/tags against the same LTS or ghc version then it is better safer to avoid using `remove-older`.

## Installation

Run `stack install` or `cabal install`

## Contributing
BSD license

Project: https://github.com/juhp/stack-clean-old

## Warning disclaimer
Use at your own risk.

The author takes no responsibility for any loss or damaged caused by using
this tool. Bug reports, suggestions, and improvements are welcome.
