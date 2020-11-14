# Revision history for stack-clean-old-work

## 0.2.1 (2020-11-11)
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
