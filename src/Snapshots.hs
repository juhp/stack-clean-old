{-# LANGUAGE CPP #-}

module Snapshots (
  --getSnapshotDirs
  --, VersionSnapshots(..)
  cleanGhcSnapshots,
  cleanMinorSnapshots,
  cleanOldStackWork,
  listGhcSnapshots,
  setStackSnapshotsDir,
  setStackWorkDir,
  sizeSnapshots,
  sizeStackWork,
  removeStackWorks
  )
where

import Control.Monad.Extra
import Data.List.Extra
import Data.Version.Extra
import SimpleCmd
#if MIN_VERSION_simple_cmd(0,2,1)
  hiding (whenM)
#endif
import System.Directory hiding (removeDirectoryRecursive, removeFile)
import System.FilePath
import Text.Printf

import Directories (globDirs, getStackSubdir, switchToSystemDirUnder)
import qualified Remove
import Versions

data SnapshotInstall =
  SnapInst { snapHash :: String,
             snapGHC :: Version}
  deriving Eq

instance Ord SnapshotInstall where
  compare s1 s2 = compare (snapGHC s1) (snapGHC s2)

data VersionSnapshots =
  VersionSnaps {snapsVersion :: Version,
                snapsHashes :: [String]}
  deriving Eq

instance Ord VersionSnapshots where
  compare s1 s2 = compare (snapsVersion s1) (snapsVersion s2)

instance Show VersionSnapshots where
  show (VersionSnaps ver snaps) = showVersion ver ++ ":" ++ show snaps

readVersionedSnaps :: [FilePath] -> [VersionSnapshots]
readVersionedSnaps snaps =
  let ghcs = (groupOn snapGHC . sort) (map readSnapshot snaps)
  in map installVerSnaps ghcs
  where
    installVerSnaps :: [SnapshotInstall] -> VersionSnapshots
    installVerSnaps versnaps =
      let ver = snapGHC (head versnaps) in
        VersionSnaps ver (map snapHash versnaps)

    readSnapshot :: FilePath -> SnapshotInstall
    readSnapshot pth =
      let (hash,ver) = splitFileName pth
      -- remove trailing '/'
      in SnapInst (init hash) (readVersion ver)

getSnapshotDirs :: Maybe Version -> IO [VersionSnapshots]
getSnapshotDirs mghcver = do
  let pat = maybe "*" versionMatch mghcver
  readVersionedSnaps <$> globDirs ("*" </> pat)
  where
    versionMatch :: Version -> String
    versionMatch ghcver =
      showVersion ghcver ++ if isMajorVersion ghcver then ".*" else ""

sizeSnapshots :: Bool -> IO ()
sizeSnapshots nothuman = do
  snapshots <- getStackSubdir "snapshots"
  cmd_ "du" $ ["-h" | not nothuman] ++ ["-s", snapshots]

listGhcSnapshots :: IO () -> Maybe Version -> IO ()
listGhcSnapshots setdir mghcver = do
  setdir
  ghcs <- getSnapshotDirs mghcver
  mapM_ printTotalGhcSize ghcs

removeVersionSnaps :: Bool -> VersionSnapshots -> IO ()
removeVersionSnaps dryrun versnap = do
  let dirs = snapsHashes versnap
  mapM_ (Remove.doRemoveDirectory dryrun) dirs
  putStrLn $ show (length dirs) ++ " snapshots " ++ (if dryrun then "would be " else "") ++ "removed for " ++ showVersion (snapsVersion versnap)

cleanGhcSnapshots :: IO () -> Bool -> Version -> IO ()
cleanGhcSnapshots setDir dryrun ghcver = do
  setDir
  ghcs <- getSnapshotDirs (Just ghcver)
  when (isMajorVersion ghcver) $ do
    Remove.prompt dryrun ("all " ++ showVersion ghcver ++ " builds")
  mapM_ (removeVersionSnaps dryrun) ghcs

cleanMinorSnapshots :: IO () -> Bool -> Maybe Version -> IO ()
cleanMinorSnapshots setDir dryrun mghcver = do
  setDir
  ghcs <- getSnapshotDirs (majorVersion <$> mghcver)
  case mghcver of
    Nothing -> do
      let majors = groupOn (majorVersion . snapsVersion) ghcs
      forM_ majors $ \ gmajor ->
        when (length gmajor > 1) $
        mapM_ (removeVersionSnaps dryrun) (init gmajor)
    Just ghcver -> do
      let newestMinor = if isMajorVersion ghcver
                        then (snapsVersion . last) ghcs
                        else ghcver
          gminors = filter ((< newestMinor) . snapsVersion) ghcs
      mapM_ (removeVersionSnaps dryrun) gminors

cleanOldStackWork :: Bool -> Int -> IO ()
cleanOldStackWork dryrun keep = do
  setStackWorkDir
  dirs <- sortOn takeFileName . lines <$> shell ( unwords $ "ls" : ["-d", "*/*"])
  let ghcs = groupOn takeFileName dirs
  mapM_ removeOlder ghcs
  where
    removeOlder :: [FilePath] -> IO ()
    removeOlder dirs = do
      let ghcver = (takeFileName . head) dirs
      oldfiles <- drop keep . reverse <$> sortedByAge
      mapM_ (Remove.doRemoveDirectory dryrun . takeDirectory) oldfiles
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

stackWorkInstall :: FilePath
stackWorkInstall = ".stack-work/install"

sizeStackWork :: Bool -> IO ()
sizeStackWork nothuman = do
  let path = stackWorkInstall
  whenM (doesDirectoryExist path) $
    cmd_ "du" $ ["-h" | not nothuman] ++ ["-s", path]

printTotalGhcSize :: VersionSnapshots -> IO ()
printTotalGhcSize versnaps = do
  total <- head . words . last <$> cmdLines "du" ("-shc":snapsHashes versnaps)
  printf "%4s  %-6s (%d dirs)\n" total ((showVersion . snapsVersion) versnaps) (length (snapsHashes versnaps))

setStackWorkDir :: IO ()
setStackWorkDir = do
  switchToSystemDirUnder stackWorkInstall

setStackSnapshotsDir :: IO ()
setStackSnapshotsDir = do
  getStackSubdir "snapshots" >>= switchToSystemDirUnder

removeStackWorks :: Bool -> IO ()
removeStackWorks dryrun = do
  workdirs <- sort <$> cmdLines "find" ["-type", "d", "-name", ".stack-work"]
  unless (null workdirs) $ do
    mapM_ putStrLn workdirs
    Remove.prompt dryrun "these dirs"
    mapM_ (Remove.doRemoveDirectory dryrun) workdirs
