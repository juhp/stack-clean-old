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
import System.Environment (lookupEnv)
import System.FilePath
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

import Directories (getStackSubdir, traversePlatforms')
import GHC
import GHCTarball
import Paths_stack_clean_old (version)
import Snapshots
import Types

data Mode = Default | Project | Snapshots | Compilers | Global | Tarballs

data Recursion = Subdirs | Recursive
  deriving Eq

main :: IO ()
main = do
  stackroot_str <- maybe "~/.stack" (const "$STACK_ROOT") <$> lookupEnv "STACK_ROOT"
  hSetBuffering stdout NoBuffering
  simpleCmdArgs (Just version) "Stack clean up tool"
    "Cleans away old stack-work builds (and pending: stack snapshots) to recover diskspace. Use the --delete option to perform actual removals. https://github.com/juhp/stack-clean-old#readme" $
    subcommands
    [ Subcommand "size" "Total size" $
      sizeCmd
      <$> modeOpt stackroot_str
      <*> recursionOpt
      <*> notHumanOpt
      <*> optional systemOpt
    , Subcommand "list" "List sizes per ghc version" $
      listCmd
      <$> modeOpt stackroot_str
      <*> recursionOpt
      <*> optional ghcVerArg
      <*> optional systemOpt
    , Subcommand "remove" "Remove for a ghc version" $
      removeCmd
      <$> deleteOpt
      <*> modeOpt stackroot_str
      <*> recursionOpt
      <*> ghcVerArg
      <*> optional systemOpt
    , Subcommand "keep-minor" "Remove for previous ghc minor versions" $
      removeMinorsCmd
      <$> deleteOpt
      <*> modeOpt stackroot_str
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
    modeOpt stackroot =
      flagWith' Project 'P' "project" "Act on current project's .stack-work/ [default in project dir]" <|>
      flagWith' Snapshots 'S' "snapshots" ("Act on" +-+ stackroot </> "snapshots/") <|>
      flagWith' Compilers 'C' "compilers" ("Act on" +-+ stackroot </> "programs/ installations") <|>
      flagWith' Tarballs 'T' "tarballs" ("Act on" +-+ stackroot </> "programs/ tarballs") <|>
      flagWith Default Global 'G' "global" ("Act on both" +-+ stackroot </> "{programs,snapshots}/ [default outside project dir]")

    deleteOpt =
      (flagWith' Delete 'd' "delete" "Do deletion [default is dryrun]" <*>
      switchWith 'y' "yes" "Assume yes for all prompts") <|>
      pure Dryrun

    recursionOpt =
      optional (
      flagWith' Subdirs 's' "subdirs" "List subdirectories"
        <|> flagWith' Recursive 'r' "recursive" "List subdirectories")

    notHumanOpt = switchWith 'H' "not-human-size"
                  "Do not use du --human-readable"

    ghcVerArg = readVersion <$> strArg "GHCVER"

    keepOption = optionalWith auto 'k' "keep" "INT"
                 "number of project builds per ghc version [default 5]" 5

    systemOpt = strOptionWith 'o' "platform" "SYSTEM"
                "Specify which OS platform to work on (eg 'x86_64-linux-tinfo6', 'aarch64-linux-nix', 'x86_64-osx', 'aarch64-osx',  etc)"

withRecursion :: Bool -> Maybe Recursion -> IO () -> IO ()
withRecursion needinstall mrecursion =
  withRecursion' True needinstall mrecursion . const

withRecursion' :: Bool -> Bool -> Maybe Recursion -> (FilePath -> IO ()) -> IO ()
withRecursion' changedir needinstall mrecursion act = do
  case mrecursion of
    Just recursion -> do
      dirs <- (if recursion == Recursive
               then map (dropPrefix "./" . takeDirectory) <$> findStackWorks
               else listStackSubdirs)
              >>= if needinstall
                  then filterM (doesDirectoryExist . (</> ".stack-work/install"))
                  else return
      forM_ dirs $ \dir ->
        if changedir
        then
          withCurrentDirectory dir $ do
          putStrLn $ "\n" ++ (if recursion == Recursive then id else takeFileName) dir ++ "/"
          act dir
        else act dir
    Nothing -> act ""

sizeCmd :: Mode -> Maybe Recursion -> Bool -> Maybe String -> IO ()
sizeCmd mode mrecursion notHuman msystem =
  case mode of
    Project -> withRecursion' False False mrecursion $ sizeStackWork notHuman
    Snapshots -> sizeSnapshots notHuman msystem
    Compilers -> sizeGhcPrograms notHuman
    Tarballs -> error' "use --compilers"
    Global -> do
      sizeCmd Snapshots Nothing notHuman msystem
      sizeCmd Compilers Nothing notHuman msystem
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject || isJust mrecursion
        then sizeCmd Project mrecursion notHuman msystem
        else sizeCmd Global Nothing notHuman msystem

listCmd :: Mode -> Maybe Recursion -> Maybe Version -> Maybe String -> IO ()
listCmd mode mrecursion mver msystem =
  withRecursion True mrecursion $
  case mode of
    Project -> listGhcSnapshots msystem mver stackWorkInstall
    Snapshots -> getStackSubdir "snapshots" >>= listGhcSnapshots msystem mver
    Compilers -> listGhcInstallation mver msystem
    Tarballs -> listGhcTarballs mver msystem
    Global -> do
      listCmd Snapshots Nothing mver msystem
      listCmd Compilers Nothing mver msystem
    Default -> do
      isProject <- doesDirectoryExist ".stack-work"
      if isProject
        then listCmd Project Nothing mver msystem
        else listCmd Global Nothing mver msystem

removeCmd :: Deletion -> Mode -> Maybe Recursion -> Version -> Maybe String
          -> IO ()
removeCmd deletion mode mrecursion ghcver msystem = do
  removeRun deletion mode mrecursion ghcver msystem
  remindDelete deletion

removeRun :: Deletion -> Mode -> Maybe Recursion -> Version -> Maybe String
          -> IO ()
removeRun deletion mode mrecursion ghcver msystem =
  withRecursion True mrecursion $
    case mode of
      Project -> do
        cwd <- getCurrentDirectory
        traversePlatforms' (return stackWorkInstall) msystem $
          cleanGhcSnapshots deletion cwd ghcver
      Snapshots -> do
        cwd <- getCurrentDirectory
        traversePlatforms' (getStackSubdir "snapshots") msystem $
          cleanGhcSnapshots deletion cwd ghcver
      Compilers -> do
        removeGhcVersionInstallation deletion ghcver msystem
      Tarballs -> do
        removeGhcVersionTarball deletion ghcver msystem
      Global -> do
        removeRun deletion Compilers Nothing ghcver msystem
        removeRun deletion Snapshots Nothing ghcver msystem
      Default -> do
        isProject <- doesDirectoryExist ".stack-work"
        if isProject
          then removeRun deletion Project Nothing ghcver msystem
          else removeRun deletion Global Nothing ghcver msystem

removeMinorsCmd :: Deletion -> Mode -> Maybe Recursion -> Maybe Version
                -> Maybe String -> IO ()
removeMinorsCmd deletion mode mrecursion mver msystem = do
  removeMinorsRun deletion mode mrecursion mver msystem
  remindDelete deletion

removeMinorsRun :: Deletion -> Mode -> Maybe Recursion -> Maybe Version
                -> Maybe String -> IO ()
removeMinorsRun deletion mode mrecursion mver msystem = do
  withRecursion True mrecursion $
    case mode of
      Project -> do
        cwd <- getCurrentDirectory
        traversePlatforms' (return stackWorkInstall) msystem $
          cleanMinorSnapshots deletion cwd mver
      Snapshots -> do
        cwd <- getCurrentDirectory
        traversePlatforms' (getStackSubdir "snapshots") msystem $
          cleanMinorSnapshots deletion cwd mver
      Compilers -> removeGhcMinorInstallation deletion mver msystem
      Tarballs -> removeGhcMinorTarball deletion mver msystem
      Global -> do
        removeMinorsRun deletion Compilers Nothing mver msystem
        removeMinorsRun deletion Snapshots Nothing mver msystem
      Default -> do
        isProject <- doesDirectoryExist ".stack-work"
        if isProject
          then removeMinorsRun deletion Project Nothing mver msystem
          else removeMinorsRun deletion Global Nothing mver msystem

purgeOlderCmd :: Deletion -> Natural -> Maybe Recursion -> Maybe String -> IO ()
purgeOlderCmd deletion keep mrecursion msystem = do
  withRecursion True mrecursion $
    cleanOldStackWork deletion keep msystem
  remindDelete deletion

deleteWorkCmd :: Deletion -> Maybe Recursion -> IO ()
deleteWorkCmd deletion mrecursion = do
  withRecursion False mrecursion $
    removeStackWork deletion
  remindDelete deletion

findStackWorks :: IO [FilePath]
findStackWorks =
  -- ignore find errors (e.g. access rights)
  L.sort . lines <$> cmdIgnoreErr "find" [".", "-type", "d", "-name", ".stack-work", "-prune"] []

listStackSubdirs :: IO [FilePath]
listStackSubdirs =
  listDirectory "." >>= filterM (doesDirectoryExist . (</> ".stack-work")) . L.sort

remindDelete :: Deletion -> IO ()
remindDelete deletion =
  unless (isDelete deletion) $ putStrLn "\n(use --delete (-d) for removal)"
