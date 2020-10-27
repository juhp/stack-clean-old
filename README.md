# stack-clean-old

A small tool to clean away older Haskell [stack](https://docs.haskellstack.org)
snapshot builds and ghc versions, to recover diskspace.

## Usage
```
stack-clean-old [project|snapshots|ghc] [size|list|remove-version|remove-earlier-minor] [GHCVER]
```
These commands act respectively on:

- the current local **project**: `.stack-work/install/`
- the user's stack **snapshot** builds: `~/.stack/snapshots/`
- installed stack **ghc** compilers: `~/.stack/programs/`.

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
    removes the builds/installs for previous minor ghc versions.
    If GHCVER is given then only minor versions older than it are removed.

NB: If you remove all snapshot builds for a version of ghc, then you would have to rebuild again for any projects still using them, so removal should be used cautiously, but it can recover a lot of diskspace.

### Example usage
To remove project builds for ghc-8.2.2:
```
$ stack-clean-old project list
154M  8.2.2  (5 dirs)
154M  8.4.4  (5 dirs)
163M  8.6.5  (5 dirs)
$ stack-clean-old project remove-version 8.2.2
```

Remove all stack ghc-8.4 snapshot builds before 8.4.4:
```
$ stack-clean-old snapshots list 8.4
421M  8.4.1  (7 dirs)
368M  8.4.2  (6 dirs)
489M  8.4.3  (8 dirs)
799M  8.4.4  (24 dirs)
$ stack-clean-old snapshots remove-earlier-minor 8.4.4
7 dirs removed for 8.4.1
6 dirs removed for 8.4.2
8 dirs removed for 8.4.3
```

Incidently I build my projects across as many major Stackage LTS versions as possible, and collectively this piles up to a lot of diskspace: so I wrote this tool to help manage that.

### Purging older stack project builds
```
stack-clean-old project remove-older
```
This command removes older stack builds from `.stack-work/install/`.
By default it keeps 5 newest builds per ghc version.

The preservation/deletion is calculated and done per ghc version.

NB: If you regularly build your project for several branches/tags against the same LTS or ghc version then it is safer to avoid using `remove-older`.

## Installation

Run `stack install` or `cabal install`

## Contributing
BSD license

Project: https://github.com/juhp/stack-clean-old

## Warning disclaimer
Use at your own risk.

The author takes no responsibility for any loss or damaged caused by using
this tool.

Bug reports, suggestions, and improvements are welcome.
