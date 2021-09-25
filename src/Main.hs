{-# LANGUAGE CPP #-}

module Main (main) where

#if !MIN_VERSION_simple_cmd_args(0,1,3)
import Control.Applicative ((<|>))
#endif
import Data.Version.Extra
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

import GHC
import Paths_stack_clean_old (version)
import Snapshots

data Mode = Default | Project | Snapshots | Compilers | GHC

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  simpleCmdArgs (Just version) "Stack clean up tool"
    "Cleans away old stack-work builds (and pending: stack snapshots) to recover diskspace." $
    subcommands
    [ Subcommand "size" "Total size" $
      sizeCmd <$> modeOpt <*> notHumanOpt
    , Subcommand "list" "List sizes per ghc version" $
      listCmd <$> modeOpt <*> optional ghcVerArg
    , Subcommand "remove" "Remove for a ghc version" $
      removeCmd <$> dryrun <*> modeOpt <*> ghcVerArg
    , Subcommand "keep-minor" "Remove for previous ghc minor versions" $
      removeMinorsCmd <$> dryrun <*> modeOpt <*> optional ghcVerArg
    , Subcommand "purge-older" "Purge older builds in .stack-work/install" $
      cleanOldStackWork <$> dryrun <*> keepOption
    , Subcommand "delete-work" "Remove project's .stack-work subdirs recursively" $
      removeStackWorks <$> dryrun <*> switchWith 'a' "all" "Find all .stack-work/ subdirs, even if current directory not a stack project"
    ]
  where
    modeOpt =
      flagWith' Project 'p' "project" "Act on current project's .stack-work/ [default in project dir]" <|>
      flagWith' GHC 'g' "global" "Act on both ~/.stack/{programs,snapshots}/ [default outside project dir]" <|>
      flagWith' Snapshots 's' "snapshots" "Act on ~/.stack/snapshots/" <|>
      flagWith Default Compilers 'c' "compilers" "Act on ~/.stack/programs/"

    dryrun = switchWith 'n' "dry-run" "Show what would be done, without removing"

    notHumanOpt = switchWith 'H' "not-human-size" "Do not use du --human-readable"

    ghcVerArg = readVersion <$> strArg "GHCVER"

    keepOption = positive <$> optionalWith auto 'k' "keep" "INT" "number of project builds per ghc version [default 5]" 5

    positive :: Int -> Int
    positive n = if n > 0 then n else error' "Must be positive integer"


sizeCmd :: Mode -> Bool -> IO ()
sizeCmd mode notHuman =
  case mode of
    Project -> sizeStackWork notHuman
    Snapshots -> sizeSnapshots notHuman
    Compilers -> sizeGhcInstalls notHuman
    GHC -> do
          sizeCmd Compilers notHuman
          sizeCmd Snapshots notHuman
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then sizeCmd Project notHuman
        else sizeCmd GHC notHuman

listCmd :: Mode -> Maybe Version -> IO ()
listCmd mode mver =
  case mode of
    Project -> setStackWorkDir >> listGhcSnapshots mver
    Snapshots -> setStackSnapshotsDir >> listGhcSnapshots mver
    Compilers -> listGhcInstallation mver
    GHC -> do
      listCmd Compilers mver
      listCmd Snapshots mver
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then listCmd Project mver
        else listCmd GHC mver

removeCmd :: Bool -> Mode -> Version -> IO ()
removeCmd dryrun mode ghcver =
  case mode of
    Project -> do
      cwd <- getCurrentDirectory
      setStackWorkDir
      cleanGhcSnapshots dryrun cwd ghcver
    Snapshots -> do
      cwd <- getCurrentDirectory
      setStackSnapshotsDir
      cleanGhcSnapshots dryrun cwd ghcver
    Compilers -> removeGhcVersionInstallation dryrun ghcver
    GHC -> do
      removeCmd dryrun Compilers ghcver
      removeCmd dryrun Snapshots ghcver
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then removeCmd dryrun Project ghcver
        else removeCmd dryrun GHC ghcver

removeMinorsCmd :: Bool -> Mode -> Maybe Version -> IO ()
removeMinorsCmd dryrun mode mver =
  case mode of
    Project -> do
      cwd <- getCurrentDirectory
      setStackWorkDir
      cleanMinorSnapshots dryrun cwd mver
    Snapshots -> do
      cwd <- getCurrentDirectory
      setStackSnapshotsDir
      cleanMinorSnapshots dryrun cwd mver
    Compilers -> removeGhcMinorInstallation dryrun mver
    GHC -> do
      removeMinorsCmd dryrun Compilers mver
      removeMinorsCmd dryrun Snapshots mver
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then removeMinorsCmd dryrun Project mver
        else removeMinorsCmd dryrun GHC mver
