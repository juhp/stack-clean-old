# Revision history for stack-clean-old-work

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
