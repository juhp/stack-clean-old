# stack-clean-old

A small tool to clean away older Haskell [stack](https://docs.haskellstack.org)
snapshot builds and ghc versions to recover diskspace.

## Usage
stack-clean-old [size|list|remove|keep-minor|purge-older|delete-work] [(-P|--project)|(-G|--global)|(-C|--compilers)|(-T|--tarballs)] [(-s|--subdirs)|(-r|--recursive)] [-d|--delete] [GHCVER]

In a project directory it acts on `.stack-work/install/` by default
(or `$STACK_WORK/install` if defined).
Otherwise on `${STACK_ROOT}/{snapshots,programs}/`
(the default *Stack root* is `~/.stack/`):
see the [Stack root documentation](https://docs.haskellstack.org/en/stable/stack_root/).

Subcommands:

`size`:
    prints the total size of `.stack-work/` of project(s) or the *Stack root*.
    directories (`size` does not take a GHCVER argument).

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

dry-run mode is used by default and one should use `--delete` (`-d`)
for actual deletion of files, after checking the dry-run output.

If you should remove any needed snapshot builds,
then they will get rebuilt again by stack next time you build any projects
needing them, so removals should be done carefully
but can recover a lot of diskspace.

Further the commands can use `--subdirs` or `--recursive` to run over
the projects in subdirs under the current directory or
all matching `.stack-work` dirs from the current directory and below
respectively.

If you have different ghc variants/archs installed
you can use `--platform` to restrict to one of then,
otherwise they are each listed by default.

### Example usage
List a project's builds:
```ShellSession
$ stack-clean-old list
149M  9.6.7  (5 dirs)
163M  9.8.4  (5 dirs)
155M  9.10.3  (5 dirs)
```
Remove project's 9.4.8 builds:
```ShellSession
$ stack-clean-old remove --delete --project 9.4.8
:
```
(--project is optional in a project dir).

Remove stack ghc-9.6 snapshot builds and minor compilers before 9.6.7:
```ShellSession
$ stack-clean-old list --global 9.6
x86_64-linux-tinfo6:
1.8G  9.6.6  (61 dirs)
279M  9.6.7  (6 dirs)
x86_64-linux:
ghc-tinfo6-9.6.6
ghc-tinfo6-9.6.7
$ stack-clean-old keep-minor --global 9.6
ghc-tinfo6-9.6.6 compiler would be removed
x86_64-linux-tinfo6:
61 dirs in ~/.stack/snapshots/x86_64-linux-tinfo6/*/9.6.6 would be removed

(use --delete (-d) for removal)
$ stack-clean-old keep-minor --global 9.6 --delete
ghc-tinfo6-9.6.6 compiler removed
x86_64-linux-tinfo6:
61 dirs in ~/.stack/snapshots/x86_64-linux-tinfo6/*/9.6.6 removed
```
(--global is optional outside a project dir).

If you have different latest minor versions for compilers and snapshots
you may prefer to specify the latest minor version to keep
to get more certain behaviour.

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

## Help output
(Note you can also run this tool via `stack clean-old`.)

`$ stack-clean-old --version`

```
0.5.2
```
To get help you can run `stack-clean-old` or:

`$ stack-clean-old --help`

```
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

### Command options
Most of the commands have similar options, e.g.:

`$ stack-clean-old remove --help`

```
Usage: stack-clean-old remove [(-d|--delete) [-y|--yes]] 
                              [(-P|--project) | (-S|--snapshots) | 
                                (-C|--compilers) | (-T|--tarballs) | 
                                (-G|--global)] 
                              [(-s|--subdirs) | (-r|--recursive)] GHCVER 
                              [-o|--platform SYSTEM]

  Remove for a ghc version

Available options:
  -d,--delete              Do deletion [default is dryrun]
  -y,--yes                 Assume yes for all prompts
  -P,--project             Act on current project's .stack-work [default in
                           project dir]
  -S,--snapshots           Act on ~/.stack/snapshots/
  -C,--compilers           Act on ~/.stack/programs/ installations
  -T,--tarballs            Act on ~/.stack/programs/ tarballs
  -G,--global              Act on both ~/.stack/{programs,snapshots}/ [default
                           outside project dir]
  -s,--subdirs             List subdirectories
  -r,--recursive           List subdirectories
  -o,--platform SYSTEM     Specify which OS platform to work on (eg
                           'x86_64-linux-tinfo6', 'aarch64-linux-nix',
                           'x86_64-osx', 'aarch64-osx', etc)
  -h,--help                Show this help text
```

(The `list` and `size` commands don't have `--delete` and `--yes`.)

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
