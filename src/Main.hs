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

data Recursion = Subdirs | Recursive
  deriving Eq

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  simpleCmdArgs (Just version) "Stack clean up tool"
    "Cleans away old stack-work builds (and pending: stack snapshots) to recover diskspace." $
    subcommands
    [ Subcommand "size" "Total size" $
      sizeCmd <$> modeOpt <*> recursionOpt <*> notHumanOpt
    , Subcommand "list" "List sizes per ghc version" $
      listCmd <$> modeOpt <*> recursionOpt <*> optional ghcVerArg
    , Subcommand "remove" "Remove for a ghc version" $
      removeCmd <$> deleteOpt <*> modeOpt <*> recursionOpt <*> ghcVerArg
    , Subcommand "keep-minor" "Remove for previous ghc minor versions" $
      removeMinorsCmd <$> deleteOpt <*> modeOpt <*> recursionOpt <*> optional ghcVerArg
    , Subcommand "purge-older" "Purge older builds in .stack-work/install" $
      purgeOlderCmd <$> deleteOpt <*> keepOption <*> recursionOpt
    , Subcommand "delete-work" "Remove project's .stack-work subdirs recursively" $
      deleteWorkCmd <$> deleteOpt <*> recursionOpt
    ]
  where
    modeOpt =
      flagWith' Project 'P' "project" "Act on current project's .stack-work/ [default in project dir]" <|>
      flagWith' GHC 'G' "global" "Act on both ~/.stack/{programs,snapshots}/ [default outside project dir]" <|>
      flagWith' Snapshots 'S' "snapshots" "Act on ~/.stack/snapshots/" <|>
      flagWith Default Compilers 'C' "compilers" "Act on ~/.stack/programs/"

    deleteOpt = flagWith Dryrun Delete 'd' "delete" "Do deletion [default is dryrun]"

    recursionOpt =
      optional (
      flagWith' Subdirs 's' "subdirs" "List subdirectories"
        <|> flagWith' Recursive 'r' "recursive" "List subdirectories")

    notHumanOpt = switchWith 'H' "not-human-size" "Do not use du --human-readable"

    ghcVerArg = readVersion <$> strArg "GHCVER"

    keepOption = optionalWith auto 'k' "keep" "INT" "number of project builds per ghc version [default 5]" 5


withRecursion :: Maybe Recursion -> IO () -> IO ()
withRecursion mrecursion act = do
  case mrecursion of
    Just recursion -> do
      dirs <- if recursion == Recursive
              then map takeDirectory <$> findStackWorks
              else listDirectory "." >>= filterM (\f -> doesDirectoryExist (f </> ".stack-work")) . L.sort
      forM_ dirs $ \dir ->
        withCurrentDirectory dir $ do
        putStrLn $ "\n" ++ takeFileName dir
        act
    Nothing -> act

sizeCmd :: Mode -> Maybe Recursion -> Bool -> IO ()
sizeCmd mode mrecursion notHuman =
  withRecursion mrecursion $
  case mode of
    Project -> sizeStackWork notHuman
    Snapshots -> sizeSnapshots notHuman
    Compilers -> sizeGhcInstalls notHuman
    GHC -> do
          sizeCmd Compilers Nothing notHuman
          sizeCmd Snapshots Nothing notHuman
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then sizeCmd Project Nothing notHuman
        else sizeCmd GHC Nothing notHuman

listCmd :: Mode -> Maybe Recursion -> Maybe Version -> IO ()
listCmd mode mrecursion mver =
  withRecursion mrecursion $
  case mode of
    Project -> setStackWorkInstallDir >> listGhcSnapshots mver
    Snapshots -> setStackSnapshotsDir >> listGhcSnapshots mver
    Compilers -> listGhcInstallation mver
    GHC -> do
      listCmd Compilers Nothing mver
      listCmd Snapshots Nothing mver
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then listCmd Project Nothing mver
        else listCmd GHC Nothing mver

removeCmd :: Deletion -> Mode -> Maybe Recursion -> Version -> IO ()
removeCmd deletion mode mrecursion ghcver =
  withRecursion mrecursion $
  case mode of
    Project -> do
      cwd <- getCurrentDirectory
      setStackWorkInstallDir
      cleanGhcSnapshots deletion cwd ghcver
    Snapshots -> do
      cwd <- getCurrentDirectory
      setStackSnapshotsDir
      cleanGhcSnapshots deletion cwd ghcver
    Compilers -> removeGhcVersionInstallation deletion ghcver
    GHC -> do
      removeCmd deletion Compilers Nothing ghcver
      removeCmd deletion Snapshots Nothing ghcver
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then removeCmd deletion Project Nothing ghcver
        else removeCmd deletion GHC Nothing ghcver

removeMinorsCmd :: Deletion -> Mode -> Maybe Recursion -> Maybe Version
                -> IO ()
removeMinorsCmd deletion mode mrecursion mver =
  withRecursion mrecursion $
  case mode of
    Project -> do
      cwd <- getCurrentDirectory
      setStackWorkInstallDir
      cleanMinorSnapshots deletion cwd mver
    Snapshots -> do
      cwd <- getCurrentDirectory
      setStackSnapshotsDir
      cleanMinorSnapshots deletion cwd mver
    Compilers -> removeGhcMinorInstallation deletion mver
    GHC -> do
      removeMinorsCmd deletion Compilers Nothing mver
      removeMinorsCmd deletion Snapshots Nothing mver
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then removeMinorsCmd deletion Project Nothing mver
        else removeMinorsCmd deletion GHC Nothing mver

purgeOlderCmd :: Deletion -> Natural -> Maybe Recursion -> IO ()
purgeOlderCmd deletion keep mrecursion =
  withRecursion mrecursion $
  cleanOldStackWork deletion keep

deleteWorkCmd :: Deletion -> Maybe Recursion -> IO ()
deleteWorkCmd deletion mrecursion =
  withRecursion mrecursion $
  removeStackWork deletion

findStackWorks :: IO [FilePath]
findStackWorks =
  -- ignore find errors (e.g. access rights)
  cmdIgnoreErr "find" [".", "-type", "d", "-name", ".stack-work", "-prune"] []
  >>= fmap L.sort . filterM (doesDirectoryExist . (</> "install")) . lines
