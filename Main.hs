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
      , Subcommand "list" "List builds in .stack-work/install per ghc version" $
        listGhcSnapshots . setStackWorkDir <$> dirOption <*> optional ghcVerArg
      , Subcommand "remove-version" "Remove builds in .stack-work/install for a ghc version" $
        cleanGhcSnapshots . setStackWorkDir <$> dirOption <*> dryrun <*> ghcVerArg
      , Subcommand "remove-earlier-minor" "Remove builds in .stack-work/install for previous ghc minor versions" $
        cleanMinorSnapshots . setStackWorkDir <$> dirOption <*> dryrun <*> optional ghcVerArg
      , Subcommand "remove-older" "Purge older builds in .stack-work/install" $
        cleanOldStackWork <$> dryrun <*> keepOption <*> optional (strArg "PROJECTDIR")
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
    dryrun = switchWith 'n' "dryrun" "Show what would be done, without removing"

    notHumanOpt = switchWith 'H' "not-human-size" "Do not use du --human-readable"

    dirOption = optional (strOptionWith 'd' "dir" "PROJECTDIR" "Path to project")
    ghcVerArg = strArg "GHCVER"

    keepOption = positive <$> optionalWith auto 'k' "keep" "INT" "number of project builds per ghc version [default 5]" 5

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

doRemoveDirectory :: Bool -> FilePath -> IO ()
doRemoveDirectory dryrun dir =
  unless dryrun $
  removeDirectoryRecursive dir

cleanGhcSnapshots :: IO () -> Bool -> String -> IO ()
cleanGhcSnapshots setDir dryrun ghcver = do
  setDir
  dirs <- takeGhcSnapshots ghcver <$> getSnapshotDirs
  mapM_ (doRemoveDirectory dryrun) dirs
  putStrLn $ show (length dirs) ++ " snapshots removed for " ++ ghcver

cleanMinorSnapshots :: IO () -> Bool -> Maybe String -> IO ()
cleanMinorSnapshots setDir dryrun mghcver = do
  setDir
  dirs <- sortOn (readVersion . takeFileName) <$> getSnapshotDirs
  case mghcver of
    Nothing -> do
      let ghcs = map (groupOn takeFileName) $ groupOn majorVersion dirs
      forM_ ghcs $ \ gmajor ->
        when (length gmajor > 1) $
        forM_ (init gmajor) $ \ gminor -> do
          mapM_ (doRemoveDirectory dryrun) gminor
          putStrLn $ show (length gminor) ++ " snapshots removed for " ++ takeFileName (head gminor)
    Just ghcver -> do
      let major =
            if length ('.' `elemIndices` ghcver) == 2
            then majorVersion ghcver
            else error' "Please specify minor version X.Y.Z"
          gmajor = groupOn takeFileName $ filter (olderMinor major (readVersion ghcver)) dirs
      when (length gmajor > 1) $
        forM_ (init gmajor) $ \ gminor -> do
          mapM_ (doRemoveDirectory dryrun) gminor
          putStrLn $ show (length gminor) ++ " snapshots removed for " ++ takeFileName (head gminor)
  where
    olderMinor :: Version -> Version -> FilePath -> Bool
    olderMinor major ghcver d =
      ((== major) . majorVersion) d &&
      ((< ghcver) . readVersion . takeFileName) d

majorVersion :: FilePath -> Version
majorVersion =
  makeVersion . init . versionBranch . readVersion . takeFileName

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

cleanOldStackWork :: Bool -> Int -> Maybe FilePath -> IO ()
cleanOldStackWork dryrun keep mdir = do
  setStackWorkDir mdir
  dirs <- sortOn takeFileName . lines <$> shell ( unwords $ "ls" : ["-d", "*/*"])
  let ghcs = groupOn takeFileName dirs
  mapM_ removeOlder ghcs
  where
    removeOlder :: [FilePath] -> IO ()
    removeOlder dirs = do
      let ghcver = (takeFileName . head) dirs
      oldfiles <- drop keep . reverse <$> sortedByAge
      mapM_ (doRemoveDirectory dryrun . takeDirectory) oldfiles
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

stackProgramsDir :: FilePath
stackProgramsDir = ".stack/programs"

sizeGhcInstalls :: Bool -> IO ()
sizeGhcInstalls nothuman = do
  home <- getHomeDirectory
  cmd_ "du" $ ["-h" | not nothuman] ++ ["-s", home </> stackProgramsDir]

setStackProgramsDir :: IO ()
setStackProgramsDir = do
  home <- getHomeDirectory
  switchToSystemDirUnder $ home </> stackProgramsDir

getGhcInstallDirs :: IO [FilePath]
getGhcInstallDirs = do
  setStackProgramsDir
  listDirectory "." >>= fmap sort . filterM doesDirectoryExist

listGhcInstallation :: Maybe String -> IO ()
listGhcInstallation mghcver = do
  dirs <- sortOn (readVersion . takeWhileEnd (/= '-')) <$> getGhcInstallDirs
  mapM_ putStrLn $ case mghcver of
    Nothing -> dirs
    Just ghcver -> filter (ghcver `isSuffixOf`) dirs

removeGhcVersionInstallation :: Bool -> String -> IO ()
removeGhcVersionInstallation dryrun ghcver = do
  dirs <- getGhcInstallDirs
  case filter (('-':ghcver) `isSuffixOf`) dirs of
    [] -> error' $ "stack ghc compiler version " ++ ghcver ++ " not found"
    [g] -> doRemoveGhcVersion dryrun g
    _ -> error' "more than one match found!!"

removeGhcMinorInstallation :: Bool -> Maybe String -> IO ()
removeGhcMinorInstallation dryrun mghcver = do
  dirs <- sortOn (readVersion . takeWhileEnd (/= '-')) <$> getGhcInstallDirs
  case mghcver of
    Nothing -> do
      let majors = groupOn (majorVersion . takeWhileEnd (/= '-')) dirs
      forM_ majors $ \ minors ->
        forM_ (init minors) $ doRemoveGhcVersion dryrun
    Just ghcver -> do
      let minors = filter ((== majorVersion ghcver) . majorVersion) dirs
      forM_ (init minors) $ doRemoveGhcVersion dryrun

doRemoveGhcVersion :: Bool -> FilePath -> IO ()
doRemoveGhcVersion dryrun ghcinst = do
  doRemoveDirectory dryrun ghcinst
  unless dryrun (removeFile (ghcinst <.> "installed"))
  putStrLn $ ghcinst ++ " removed"
