# Release history for stack-clean-old

## 0.5.2 (2025-12-28)
- 'delete-work' now provides output
- some safe refactorings (helps with #21 reported by @andreasabel)
- support `$STACK_WORK` (#20 suggested by @JonnyRa)

## 0.5.1 (2023-12-17)
- listStackSubdirs: sort after filtering
- Directory: ignore non-directories, like .DS_Store files (#19,
  thanks to @0rphee for reporting #18)

## 0.5 (2023-09-13)
- simpler dry run output and use simple-prompt yesNo
- add --yes option for --delete
- keep-minor: fix ghc minor version checks with major version

## 0.4.8 (2023-08-26)
- support the `STACK_ROOT` environment variable which overrides
  the default Stack root (`~/.stack`) by @PRESFIL (#12)
- list all the platform variants by default (#15)

## 0.4.7 (2023-08-22)
- add --tarballs for programs/ ghc tarballs too (#9)
- bug fix to match both ghc-*.* and ghc-variant-*.* (#14)
- rename --os-system to --platform and add more explanation (#15)
- parse ghc installation versions more carefully
- ignore hidden files in stack dirs

## 0.4.6 (2022-02-03)
- fix --recursive and --subdirs to work again
- show --recursive dir paths
- ignore non-ghc dirs in ~/.stack/programs/

## 0.4.5 (2022-01-14)
- fix multiple reminders about using --delete

## 0.4.4 (2022-01-04)
- handle .stack-work/ consistently for --subdir and --recursive
- remind user to use '--delete' for removal
- purge-older: output is now sorted by ghc versions

## 0.4.3 (2021-11-23)
- 'delete-work' now prints ".stack-work" would be deleted
- prompts now print what would be deleted
- '--recursive' now prints dirs with '/' appended

## 0.4.2 (2021-11-22)
- add optional --os-system to fix #7
- list/size snapshots before compilers for --global

## 0.4.1 (2021-10-05)
- --help now mentions --delete and a link to README

## 0.4 (2021-09-26)
- dryrun is now default: use --delete for actual removal
  (suggested by @andreasabel #6)
- new --subdirs and --recursive options
- various output improvements
- purge-older now also says "would be removed" when dryrun

## 0.3.1 (2021-08-01)
- 'delete-work': use find -prune and ignore inaccessible files (@petrem, #4)

## 0.3 (2021-01-08)
- drop subsubcommands to simplify UI
- default to project if there is a .stack-work/ dir, otherwise global ~/.stack
- rename commands:
  - remove-version -> remove
  - remove-earlier-minor -> keep-minor
  - remove-older -> purge-older
  - remove-work -> delete-work
- fix handling of partially installed ghc compiler temp dirs (#2)
- rename --dryrun to --dry-run (#1)
- drop --dir option
- delete-work: use --all to run from a non-project dir

## 0.2.2 (2002-12-31)
- add 'project remove-work' to recursively remove .stack-work from projects

## 0.2.1 (2020-11-14)
- remove-earlier-minor can now take a major version
- major internal refactors with VersionSnapshots type
- split code into modules

## 0.2 (2020-10-26)
- add --dryrun to all remove commands
- remove-earlier-minor subcommands to purge for previous ghc minor versions
- allow major ghc X.Y versions (prompts for removal)
- 'ghc list' is now sorted

## 0.1 (2020-09-22)
- initial release with project and snapshots subcommands
