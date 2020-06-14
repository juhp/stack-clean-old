# stack-clean-old

A little tool to clean away older stack builds of a project and cached snapshots.

## Usage
In a project run

```
$ stack-clean-old project
```

to remove older stack builds from `.stack-work/install/`.
It keeps 4 builds per ghc version.

To cut down on old cached snapshot builds:

```
$ stack-clean-old snapshots
```
It keeps 50 snapshot builds per ghc version.

## Installation

Run `stack install` or `cabal new-install`! ;-)

## Notes

Perservation/deletion is done by ghc version.
