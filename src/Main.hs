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
import Types

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
      removeCmd <$> deleteOpt <*> modeOpt <*> ghcVerArg
    , Subcommand "keep-minor" "Remove for previous ghc minor versions" $
      removeMinorsCmd <$> deleteOpt <*> modeOpt <*> optional ghcVerArg
    , Subcommand "purge-older" "Purge older builds in .stack-work/install" $
      cleanOldStackWork <$> deleteOpt <*> keepOption
    , Subcommand "delete-work" "Remove project's .stack-work subdirs recursively" $
      removeStackWorks <$> deleteOpt <*> switchWith 'a' "all" "Find all .stack-work/ subdirs, even if current directory not a stack project"
    ]
  where
    modeOpt =
      flagWith' Project 'p' "project" "Act on current project's .stack-work/ [default in project dir]" <|>
      flagWith' GHC 'g' "global" "Act on both ~/.stack/{programs,snapshots}/ [default outside project dir]" <|>
      flagWith' Snapshots 's' "snapshots" "Act on ~/.stack/snapshots/" <|>
      flagWith Default Compilers 'c' "compilers" "Act on ~/.stack/programs/"

    deleteOpt = flagWith Dryrun Delete 'd' "delete" "Without this option dryrun is done"

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

removeCmd :: Deletion -> Mode -> Version -> IO ()
removeCmd deletion mode ghcver =
  case mode of
    Project -> do
      cwd <- getCurrentDirectory
      setStackWorkDir
      cleanGhcSnapshots deletion cwd ghcver
    Snapshots -> do
      cwd <- getCurrentDirectory
      setStackSnapshotsDir
      cleanGhcSnapshots deletion cwd ghcver
    Compilers -> removeGhcVersionInstallation deletion ghcver
    GHC -> do
      removeCmd deletion Compilers ghcver
      removeCmd deletion Snapshots ghcver
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then removeCmd deletion Project ghcver
        else removeCmd deletion GHC ghcver

removeMinorsCmd :: Deletion -> Mode -> Maybe Version -> IO ()
removeMinorsCmd deletion mode mver =
  case mode of
    Project -> do
      cwd <- getCurrentDirectory
      setStackWorkDir
      cleanMinorSnapshots deletion cwd mver
    Snapshots -> do
      cwd <- getCurrentDirectory
      setStackSnapshotsDir
      cleanMinorSnapshots deletion cwd mver
    Compilers -> removeGhcMinorInstallation deletion mver
    GHC -> do
      removeMinorsCmd deletion Compilers mver
      removeMinorsCmd deletion Snapshots mver
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then removeMinorsCmd deletion Project mver
        else removeMinorsCmd deletion GHC mver
