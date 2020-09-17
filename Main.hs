{-# LANGUAGE CPP #-}

import Control.Monad.Extra
import Data.List.Extra
import Data.Maybe
import Data.Version.Extra
import SimpleCmd
#if MIN_VERSION_simple_cmd(0,2,1)
  hiding (ifM, whenM)
#endif
import SimpleCmdArgs
import System.Directory
import System.FilePath
import Text.Printf

import Paths_stack_clean_old (version)

main :: IO ()
main = do
  simpleCmdArgs (Just version) "Stack clean up tool"
    "Cleans away old stack-work builds (and pending: stack snapshots) to recover diskspace." $
    subcommands
    [ Subcommand "project" "Commands for project .stack-work builds" $
      subcommands
      [ Subcommand "size" "Total size of project's .stack-work/install" $
        sizeStackWork <$> dirOption <*> notHumanOpt
      , Subcommand "list" "list builds in .stack-work/install per ghc version" $
        listGhcSnapshots . setStackWorkDir <$> dirOption <*> optional ghcVerArg
      , Subcommand "remove-version" "remove builds in .stack-work/install for a ghc version" $
        cleanGhcSnapshots . setStackWorkDir <$> dirOption <*> ghcVerArg
      , Subcommand "remove-older" "purge older builds in .stack-work/install" $
        cleanOldStackWork <$> keepOption <*> optional (strArg "PROJECTDIR")
      ]
    , Subcommand "snapshots" "Commands for ~/.stack/snapshots" $
      subcommands
      [ Subcommand "size" "Total size of all stack build snapshots" $
        sizeSnapshots <$> notHumanOpt
      , Subcommand "list" "List build snapshots per ghc version" $
        listGhcSnapshots setStackSnapshotsDir <$> optional ghcVerArg
      , Subcommand "remove-version" "Remove build snapshots for a ghc version" $
        cleanGhcSnapshots setStackSnapshotsDir <$> ghcVerArg
      ]
    ]
  where
    notHumanOpt = switchWith 'H' "not-human-size" "Do not use du --human-readable"

    dirOption = optional (strOptionWith 'd' "dir" "PROJECTDIR" "Path to project")
    ghcVerArg = strArg "GHCVER"

    keepOption = positive <$> optionalWith auto 'k' "keep" "INT" "number of project builds per ghc version" 5

    positive :: Int -> Int
    positive n = if n > 0 then n else error' "Must be positive integer"

stackWorkInstall :: FilePath
stackWorkInstall = ".stack-work/install"

sizeStackWork :: Maybe FilePath -> Bool -> IO ()
sizeStackWork mdir nothuman = do
  let path = fromMaybe "" mdir </> stackWorkInstall
  whenM (doesDirectoryExist path) $
    cmd_ "du" $ ["-h" | not nothuman] ++ ["-s", path]

printTotalGhcSize :: [FilePath] -> IO ()
printTotalGhcSize ds = do
  total <- head . words . last <$> cmdLines "du" ("-shc":ds)
  printf "%4s  %-6s (%d dirs)\n" total ((takeFileName . head) ds) (length ds)

setStackWorkDir :: Maybe FilePath -> IO ()
setStackWorkDir mdir = do
  whenJust mdir $ \ dir -> setCurrentDirectory dir
  switchToSystemDirUnder stackWorkInstall

setStackSnapshotsDir :: IO ()
setStackSnapshotsDir = do
  home <- getHomeDirectory
  switchToSystemDirUnder $ home </> ".stack/snapshots"

getSnapshotDirs :: IO [FilePath]
getSnapshotDirs = do
  lines <$> shell ( unwords $ "ls" : ["-d", "*/*"])

takeGhcSnapshots :: String -> [FilePath] -> [FilePath]
takeGhcSnapshots ghcver =
  map takeDirectory . filter ((== ghcver) . takeFileName)

sizeSnapshots :: Bool -> IO ()
sizeSnapshots nothuman = do
  home <- getHomeDirectory
  cmd_ "du" $ ["-h" | not nothuman] ++ ["-s", home </> ".stack/snapshots"]

listGhcSnapshots :: IO () -> Maybe String -> IO ()
listGhcSnapshots setdir mghcver = do
  setdir
  dirs <- sortOn (readVersion . takeFileName) <$> getSnapshotDirs
  case mghcver of
    Nothing -> do
      let ghcs = groupOn takeFileName dirs
      mapM_ printTotalGhcSize ghcs
    Just ghcver -> do
      let ds = takeGhcSnapshots ghcver dirs
      unless (null dirs) $
        cmd_ "du" ("-shc":ds)

cleanGhcSnapshots :: IO () -> String -> IO ()
cleanGhcSnapshots setDir ghcver = do
  setDir
  dirs <- takeGhcSnapshots ghcver <$> getSnapshotDirs
  mapM_ removeDirectoryRecursive dirs
  putStrLn $ show (length dirs) ++ " snapshots removed for " ++ ghcver

switchToSystemDirUnder :: FilePath -> IO ()
switchToSystemDirUnder dir = do
  ifM (doesDirectoryExist dir)
    (setCurrentDirectory dir)
    (error' $ dir ++ "not found")
  systems <- listDirectory "."
  let system = case systems of
        [] -> error' $ "No OS system in " ++ dir
        [sys] -> sys
        _ -> error' "More than one OS systems found " ++ dir ++ " (unsupported)"
  setCurrentDirectory system

cleanOldStackWork :: Int -> Maybe FilePath -> IO ()
cleanOldStackWork keep mdir = do
  setStackWorkDir mdir
  dirs <- sortOn takeFileName . lines <$> shell ( unwords $ "ls" : ["-d", "*/*"])
  let ghcs = groupOn takeFileName dirs
  mapM_ removeOlder ghcs
  where
    removeOlder :: [FilePath] -> IO ()
    removeOlder dirs = do
      let ghcver = (takeFileName . head) dirs
      oldfiles <- drop keep . reverse <$> sortedByAge
      mapM_ (removeDirectoryRecursive . takeDirectory) oldfiles
      unless (null oldfiles) $
        putStrLn $ show (length oldfiles) ++ " dirs removed for " ++ ghcver
      where
        sortedByAge = do
          fileTimes <- mapM newestTimeStamp dirs
          return $ map fst $ sortOn snd fileTimes

        newestTimeStamp dir = do
          withCurrentDirectory dir $ do
            files <- listDirectory "."
            timestamp <- maximum <$> mapM getModificationTime files
            return (dir, timestamp)
