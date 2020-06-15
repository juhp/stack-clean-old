# stack-clean-old

A little tool to clean away older stack builds of a project.

## Usage
In a project run

```
$ stack-clean-old project
```

to remove older stack builds from `.stack-work/install/`.
By default it keeps 4 builds per ghc version.

## Installation

Run `stack install` or `cabal new-install`! ;-)

## Notes

Preservation/deletion is done by ghc version.

## Warning
Use at your own risk.
