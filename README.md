# stack-clean-old

A small tool to clean away older Haskell [stack](https://docs.haskellstack.org)
snapshot builds and ghc versions to recover diskspace.

## Usage
```
stack-clean-old [size|list|remove|keep-minor|purge-older|delete-work] [(-p|--project) | (-g|--ghc)] [GHCVER]
```
Options:

- `--project` (default in a stack project dir): act on `.stack-work/install/`
- `--global` (default outside a project dir): act on `~/.stack/snapshots/` and `~/.stack/programs/`

and the subcommands:

`size`:
    prints the total size of the above directories
    (`size` does not take a GHCVER argument).

`list`:
    shows the total size and number of snapshots per ghc version
    (the GHCVER argument is optional).

`remove`:
    removes for the specified ghc version (the GHCVER argument is required).

`keep-minor`:
    removes the builds/installs for previous minor ghc versions.
    If GHCVER is given then only minor versions older than it are removed.

`purge-older` and `delete-work`:
    see sections below

If you remove any needed snapshot builds for a version of ghc,
then you would have to rebuild them again for any projects still using them,
so removal should be used cautiously, but it can recover a lot of diskspace.

NBB: All the command support `--dry-run` (`-n`), so please use it to
check the effect of running them safely beforehand.

### Example usage
To remove project builds for ghc-8.2.2:
```
$ stack-clean-old list
154M  8.2.2  (5 dirs)
154M  8.4.4  (5 dirs)
163M  8.6.5  (5 dirs)
$ stack-clean-old remove -p 8.2.2
```

Remove stack ghc-8.4 snapshot builds and compilers before 8.4.4:
```
$ stack-clean-old list -g 8.4
421M  8.4.1  (7 dirs)
368M  8.4.2  (6 dirs)
489M  8.4.3  (8 dirs)
799M  8.4.4  (24 dirs)
$ stack-clean-old keep-minor -g 8.4.4
ghc-tinfo6-8.4.3 removed
7 dirs removed for 8.4.1
6 dirs removed for 8.4.2
8 dirs removed for 8.4.3
```

Incidently I build my projects across as many major Stackage LTS versions as possible (using my stack-all tool), and collectively this piles up to a lot of diskspace: so I wrote this tool to help manage that.

### Purging older stack project builds
```
stack-clean-old purge-older
```
This command removes older stack builds from `.stack-work/install/`.
By default it keeps 5 newest builds per ghc version.

The preservation/deletion is calculated and done per ghc version.

NB: If you regularly build your project for several branches/tags against the same LTS or ghc version then it is safer to avoid using `purge-older`.

### Deleting all `.stack-work/` subdirectories
`stack-clean-old delete-work` can be used to recursively remove
_all_ `.stack-work/` dirs from a project to save space
(seems same as `stack clean --full`).

`stack-clean-old delete-work --all` works from outside stack projects:
please test with `--dry-run` first.

## Installation

Run `stack install` or `cabal install`

## Related
This tool complements [stack-all](https://hackage.haskell.org/package/stack-all)
which builds projects over LTS major versions and hence generates lot of stack builds.

## Contributing
BSD license

Project: https://github.com/juhp/stack-clean-old

## Warning disclaimer
Use at your own risk: the author takes no responsibility for any loss or damaged caused by using this tool, as also mentioned in LICENSE.

Bug reports, suggestions, and improvements are welcome.
