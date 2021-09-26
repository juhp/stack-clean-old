{-# LANGUAGE CPP #-}

module Main (main) where

#if !MIN_VERSION_simple_cmd_args(0,1,3)
import Control.Applicative ((<|>))
#endif
import Control.Monad
import qualified Data.List as L
import Data.Version.Extra
import Numeric.Natural
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.FilePath
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
      listCmd <$> modeOpt <*> subdirsOpt <*> optional ghcVerArg
    , Subcommand "remove" "Remove for a ghc version" $
      removeCmd <$> deleteOpt <*> modeOpt <*> subdirsOpt <*> ghcVerArg
    , Subcommand "keep-minor" "Remove for previous ghc minor versions" $
      removeMinorsCmd <$> deleteOpt <*> modeOpt <*> subdirsOpt <*> optional ghcVerArg
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

    subdirsOpt = switchWith 'S' "subdirs" "List subdirectories"

    notHumanOpt = switchWith 'H' "not-human-size" "Do not use du --human-readable"

    ghcVerArg = readVersion <$> strArg "GHCVER"

    keepOption = optionalWith auto 'k' "keep" "INT" "number of project builds per ghc version [default 5]" 5

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

withSubdirs :: IO () -> IO ()
withSubdirs act = do
  ls <- L.sort <$> listDirectory "."
  dirs <- filterM (\f -> doesDirectoryExist (f </> ".stack-work")) ls
  forM_ dirs $ \dir ->
    withCurrentDirectory dir $ do
      putStrLn $ "\n" ++ takeFileName dir
      act

listCmd :: Mode -> Bool -> Maybe Version -> IO ()
listCmd mode subdirs mver =
  if subdirs
  then withSubdirs $ listCmd mode False mver
  else
    case mode of
      Project -> setStackWorkDir >> listGhcSnapshots mver
      Snapshots -> setStackSnapshotsDir >> listGhcSnapshots mver
      Compilers -> listGhcInstallation mver
      GHC -> do
        listCmd Compilers False mver
        listCmd Snapshots False mver
      Default -> do
        isProject <- doesDirectoryExist ".stack-work"
        if isProject
          then listCmd Project subdirs mver
          else listCmd GHC False mver

removeCmd :: Deletion -> Mode -> Bool -> Version -> IO ()
removeCmd deletion mode subdirs ghcver =
  if subdirs
  then withSubdirs $ removeCmd deletion mode False ghcver
  else
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
      removeCmd deletion Compilers False ghcver
      removeCmd deletion Snapshots False ghcver
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then removeCmd deletion Project False ghcver
        else removeCmd deletion GHC False ghcver

removeMinorsCmd :: Deletion -> Mode -> Bool -> Maybe Version -> IO ()
removeMinorsCmd deletion mode subdirs mver =
  if subdirs
  then withSubdirs $ removeMinorsCmd deletion mode False mver
  else
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
        removeMinorsCmd deletion Compilers False mver
        removeMinorsCmd deletion Snapshots False mver
      Default -> do
        isProject <- doesDirectoryExist ".stack-work"
        if isProject
          then removeMinorsCmd deletion Project False mver
          else removeMinorsCmd deletion GHC False mver
