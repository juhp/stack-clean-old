{-# LANGUAGE CPP #-}

module Main (main) where

#if !MIN_VERSION_simple_cmd_args(0,1,3)
import Control.Applicative ((<|>))
#endif
import Control.Monad
import qualified Data.List as L
import Data.List.Extra
import Data.Maybe
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
    "Cleans away old stack-work builds (and pending: stack snapshots) to recover diskspace. Use the --delete option to perform actual removals. https://github.com/juhp/stack-clean-old#readme" $
    subcommands
    [ Subcommand "size" "Total size" $
      sizeCmd
      <$> modeOpt
      <*> recursionOpt
      <*> notHumanOpt
    , Subcommand "list" "List sizes per ghc version" $
      listCmd
      <$> modeOpt
      <*> recursionOpt
      <*> optional ghcVerArg
      <*> optional systemOpt
    , Subcommand "remove" "Remove for a ghc version" $
      removeCmd
      <$> deleteOpt
      <*> modeOpt
      <*> recursionOpt
      <*> ghcVerArg
      <*> optional systemOpt
    , Subcommand "keep-minor" "Remove for previous ghc minor versions" $
      removeMinorsCmd
      <$> deleteOpt
      <*> modeOpt
      <*> recursionOpt
      <*> optional ghcVerArg
      <*> optional systemOpt
    , Subcommand "purge-older" "Purge older builds in .stack-work/install" $
      purgeOlderCmd
      <$> deleteOpt
      <*> keepOption
      <*> recursionOpt
      <*> optional systemOpt
    , Subcommand "delete-work" "Remove project's .stack-work/ (optionally recursively)" $
      deleteWorkCmd
      <$> deleteOpt
      <*> recursionOpt
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

    notHumanOpt = switchWith 'H' "not-human-size"
                  "Do not use du --human-readable"

    ghcVerArg = readVersion <$> strArg "GHCVER"

    keepOption = optionalWith auto 'k' "keep" "INT"
                 "number of project builds per ghc version [default 5]" 5

    systemOpt = strOptionWith 'o' "os-system" "SYSTEM"
                "Specify which of the OS platforms to work on (eg 'x86_64-linux-tinfo6' or 'aarch64-linux-nix', etc)"

withRecursion :: Maybe Recursion -> IO () -> IO ()
withRecursion mrecursion act = do
  case mrecursion of
    Just recursion -> do
      dirs <- if recursion == Recursive
              then map takeDirectory <$> findStackWorks
              else listStackSubdirs
      forM_ dirs $ \dir ->
        withCurrentDirectory dir $ do
        putStrLn $ "\n" ++ takeFileName dir
        act
    Nothing -> act

withRecursion' :: Maybe Recursion -> (FilePath -> IO ()) -> IO ()
withRecursion' mrecursion act = do
  case mrecursion of
    Just recursion -> do
      dirs <- if recursion == Recursive
              then map (dropPrefix "./" . takeDirectory) <$> findStackWorks
              else listStackSubdirs
      mapM_ act dirs
    Nothing -> act ""

sizeCmd :: Mode -> Maybe Recursion -> Bool -> IO ()
sizeCmd mode mrecursion notHuman =
  withRecursion' mrecursion $ \dir ->
  case mode of
    Project -> sizeStackWork notHuman dir
    Snapshots -> sizeSnapshots notHuman
    Compilers -> sizeGhcInstalls notHuman
    GHC -> do
      sizeCmd Compilers Nothing notHuman
      sizeCmd Snapshots Nothing notHuman
    Default ->
      if isJust mrecursion
      then sizeStackWork notHuman dir
      else do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then sizeCmd Project Nothing notHuman
        else sizeCmd GHC Nothing notHuman

listCmd :: Mode -> Maybe Recursion -> Maybe Version -> Maybe String -> IO ()
listCmd mode mrecursion mver msystem =
  withRecursion mrecursion $
  case mode of
    Project -> setStackWorkInstallDir msystem >> listGhcSnapshots mver
    Snapshots -> setStackSnapshotsDir msystem >> listGhcSnapshots mver
    Compilers -> listGhcInstallation mver msystem
    GHC -> do
      listCmd Compilers Nothing mver msystem
      listCmd Snapshots Nothing mver msystem
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then listCmd Project Nothing mver msystem
        else listCmd GHC Nothing mver msystem

removeCmd :: Deletion -> Mode -> Maybe Recursion -> Version -> Maybe String
          -> IO ()
removeCmd deletion mode mrecursion ghcver msystem =
  withRecursion mrecursion $
  case mode of
    Project -> do
      cwd <- getCurrentDirectory
      setStackWorkInstallDir msystem
      cleanGhcSnapshots deletion cwd ghcver
    Snapshots -> do
      cwd <- getCurrentDirectory
      setStackSnapshotsDir msystem
      cleanGhcSnapshots deletion cwd ghcver
    Compilers -> removeGhcVersionInstallation deletion ghcver msystem
    GHC -> do
      removeCmd deletion Compilers Nothing ghcver msystem
      removeCmd deletion Snapshots Nothing ghcver msystem
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then removeCmd deletion Project Nothing ghcver msystem
        else removeCmd deletion GHC Nothing ghcver msystem

removeMinorsCmd :: Deletion -> Mode -> Maybe Recursion -> Maybe Version
                -> Maybe String -> IO ()
removeMinorsCmd deletion mode mrecursion mver msystem =
  withRecursion mrecursion $
  case mode of
    Project -> do
      cwd <- getCurrentDirectory
      setStackWorkInstallDir msystem
      cleanMinorSnapshots deletion cwd mver
    Snapshots -> do
      cwd <- getCurrentDirectory
      setStackSnapshotsDir msystem
      cleanMinorSnapshots deletion cwd mver
    Compilers -> removeGhcMinorInstallation deletion mver msystem
    GHC -> do
      removeMinorsCmd deletion Compilers Nothing mver msystem
      removeMinorsCmd deletion Snapshots Nothing mver msystem
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then removeMinorsCmd deletion Project Nothing mver msystem
        else removeMinorsCmd deletion GHC Nothing mver msystem

purgeOlderCmd :: Deletion -> Natural -> Maybe Recursion -> Maybe String -> IO ()
purgeOlderCmd deletion keep mrecursion msystem =
  withRecursion mrecursion $
  cleanOldStackWork deletion keep msystem

deleteWorkCmd :: Deletion -> Maybe Recursion -> IO ()
deleteWorkCmd deletion mrecursion =
  withRecursion mrecursion $
  removeStackWork deletion

findStackWorks :: IO [FilePath]
findStackWorks =
  -- ignore find errors (e.g. access rights)
  cmdIgnoreErr "find" [".", "-type", "d", "-name", ".stack-work", "-prune"] []
  >>= fmap L.sort . filterM (doesDirectoryExist . (</> "install")) . lines

listStackSubdirs :: IO [FilePath]
listStackSubdirs =
  listDirectory "." >>= filterM (\f -> doesDirectoryExist (f </> ".stack-work")) . L.sort
