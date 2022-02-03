# stack-clean-old

A small tool to clean away older Haskell [stack](https://docs.haskellstack.org)
snapshot builds and ghc versions to recover diskspace.

## Usage
`stack-clean-old [size|list|remove|keep-minor|purge-older|delete-work] [(-P|--project)|(-G|--global)] [(-s|--subdirs)|(-r|--recursive)] [-d|--delete] [GHCVER]`

In a project directory it acts on `.stack-work/install/` by default,
otherwise on `~/.stack/{snapshots,programs}/`.

Subcommands:

`size`:
    prints the total size of `.stack-work/` or `~/.stack/`
    (`size` does not take a GHCVER argument).

`list`:
    shows the total size and number of snapshots per ghc version
    (the GHCVER argument is optional).

`remove`:
    removes for the specified ghc version (the GHCVER argument is required).

`keep-minor`:
    removes the builds/installs for older minor releases of ghc major versions.
    If GHCVER is given then only minor versions older than it
    (or than the latest installed minor version) are removed.
    If no GHCVER is given it applies to each installed ghc major version.

`purge-older`:
    removes snapshot builds with older timestamps

`delete-work`:
    removes `.stack-work` directories completely

Since version 0.4 dry-run mode is now the default and one needs to use
`--delete` (`-d`) for actual deletion of files,
after checking the dry-run output first.

If you should remove any needed snapshot builds,
then they will get rebuilt again by stack next time you build any projects
using them, so removals should be done carefully
but can recover a lot of diskspace.

Further the commands can use `--subdirs` or `--recursive` to run over
the projects in subdirs under the current directory or
all matching `.stack-work` dirs from the current directory and below
respectively.

### Example usage
List a project's builds:
```ShellSession
$ stack-clean-old list
154M  8.2.2  (5 dirs)
154M  8.4.4  (5 dirs)
163M  8.6.5  (5 dirs)
```
Remove project's 8.2.2 builds:
```ShellSession
$ stack-clean-old remove --delete --project 8.2.2
:
```
(--project is optional in a project dir).

Remove stack ghc-8.4 snapshot builds and minor compilers before 8.4.4:
```ShellSession
$ stack-clean-old list --global 8.4
421M  8.4.1  (7 dirs)
368M  8.4.2  (6 dirs)
489M  8.4.3  (8 dirs)
799M  8.4.4  (24 dirs)
$ stack-clean-old keep-minor -d -g 8.4.4
ghc-tinfo6-8.4.3 removed
7 dirs removed for 8.4.1
6 dirs removed for 8.4.2
8 dirs removed for 8.4.3
```
(--global is optional outside a project dir).

### Purging older stack project builds
```
stack-clean-old purge-older
```
This command removes older stack builds from `.stack-work/install/`.
By default it keeps 5 newest builds per ghc version.

The preservation/deletion is calculated and done per ghc version.

NB: If you regularly build your project for several branches/tags against the same LTS or ghc version then it is safer to avoid using `purge-older`.

### Deleting all `.stack-work/` subdirectories
`stack-clean-old delete-work --recursive` can be used to remove recursively
_all_ `.stack-work/` dirs within (or outside) a project directory to save
space (seems same as `stack clean --full` inside a project).

### Help output
(Note you can also run this tool via `stack clean-old`.)

To get help you can run `stack-clean-old --help` or just:
```ShellSession
$ stack-clean-old
Stack clean up tool

Usage: stack-clean-old [--version] COMMAND
  Cleans away old stack-work builds (and pending: stack snapshots) to recover
  diskspace. Use the --delete option to perform actual removals.
  https://github.com/juhp/stack-clean-old#readme

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  size                     Total size
  list                     List sizes per ghc version
  remove                   Remove for a ghc version
  keep-minor               Remove for previous ghc minor versions
  purge-older              Purge older builds in .stack-work/install
  delete-work              Remove project's .stack-work/ (optionally
                           recursively)
```

## Installation

Run `stack install` or `cabal install`

## Related
This tool complements
[stack-all](https://hackage.haskell.org/package/stack-all)
which builds projects across LTS major versions and
hence generates a lot of stack builds.

[cabal-clean](https://hackage.haskell.org/package/cabal-clean) is
a similar tool for cleaning old cached cabal build files.

## Contributing
BSD license

Project: <https://github.com/juhp/stack-clean-old>

## Disclaimer
Use at your own risk: the author takes no responsibility for any loss or
damaged caused by using this tool.

Bug reports, suggestions, and improvements are welcome.
