module Main (main) where

import Data.Version.Extra
import SimpleCmd
import SimpleCmdArgs
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

import GHC
import Paths_stack_clean_old (version)
import Snapshots

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  simpleCmdArgs (Just version) "Stack clean up tool"
    "Cleans away old stack-work builds (and pending: stack snapshots) to recover diskspace." $
    subcommands
    [ Subcommand "project" "Commands for project .stack-work builds" $
      subcommands
      [ Subcommand "size" "Total size of project's .stack-work/install" $
        sizeStackWork <$> dirOption <*> notHumanOpt
      , Subcommand "list" "List builds in .stack-work/install per ghc version" $
        listGhcSnapshots . setStackWorkDir <$> dirOption <*> optional ghcVerArg
      , Subcommand "remove-version" "Remove builds in .stack-work/install for a ghc version" $
        cleanGhcSnapshots . setStackWorkDir <$> dirOption <*> dryrun <*> ghcVerArg
      , Subcommand "remove-earlier-minor" "Remove builds in .stack-work/install for previous ghc minor versions" $
        cleanMinorSnapshots . setStackWorkDir <$> dirOption <*> dryrun <*> optional ghcVerArg
      , Subcommand "remove-older" "Purge older builds in .stack-work/install" $
        cleanOldStackWork <$> dryrun <*> keepOption <*> optional (strArg "PROJECTDIR")
      , Subcommand "remove-work" "Remove .stack-work subdirs recursively" $
        removeStackWorks <$> dryrun <*> optional (strArg "PROJECTDIR")
      ]
    , Subcommand "snapshots" "Commands for ~/.stack/snapshots" $
      subcommands
      [ Subcommand "size" "Total size of all stack build snapshots" $
        sizeSnapshots <$> notHumanOpt
      , Subcommand "list" "List build snapshots per ghc version" $
        listGhcSnapshots setStackSnapshotsDir <$> optional ghcVerArg
      , Subcommand "remove-version" "Remove build snapshots for a ghc version" $
        cleanGhcSnapshots setStackSnapshotsDir <$> dryrun <*> ghcVerArg
      , Subcommand "remove-earlier-minor" "Remove build snapshots for previous ghc minor versions" $
        cleanMinorSnapshots setStackSnapshotsDir <$> dryrun <*> optional ghcVerArg
      ]
    , Subcommand "ghc" "Commands on stack's ghc compiler installations" $
      subcommands
      [ Subcommand "size" "Total size of installed stack ghc compilers" $
        sizeGhcInstalls <$> notHumanOpt
      , Subcommand "list" "List installed stack ghc compiler versions" $
        listGhcInstallation <$> optional ghcVerArg
      , Subcommand "remove-version" "Remove installation of a stack ghc compiler version" $
        removeGhcVersionInstallation <$> dryrun <*> ghcVerArg
      , Subcommand "remove-earlier-minor" "Remove installations of stack ghc previous minor versions" $
        removeGhcMinorInstallation <$> dryrun <*> optional ghcVerArg
      ]
    ]
  where
    dryrun = switchWith 'n' "dry-run" "Show what would be done, without removing"

    notHumanOpt = switchWith 'H' "not-human-size" "Do not use du --human-readable"

    dirOption = optional (strOptionWith 'd' "dir" "PROJECTDIR" "Path to project")
    ghcVerArg = readVersion <$> strArg "GHCVER"

    keepOption = positive <$> optionalWith auto 'k' "keep" "INT" "number of project builds per ghc version [default 5]" 5

    positive :: Int -> Int
    positive n = if n > 0 then n else error' "Must be positive integer"
