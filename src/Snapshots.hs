{-# LANGUAGE CPP #-}

module Snapshots (
  cleanGhcSnapshots,
  cleanMinorSnapshots,
  cleanOldStackWork,
  listGhcSnapshots,
  stackWorkInstall,
  sizeSnapshots,
  sizeStackWork,
  removeStackWork
  )
where

import Control.Monad.Extra
import Data.List.Extra
import Data.Version.Extra
import Numeric.Natural
import SimpleCmd
#if MIN_VERSION_simple_cmd(0,2,1)
  hiding (whenM)
#endif
import System.Directory hiding (removeDirectoryRecursive, removeFile)
import System.FilePath
import Text.Printf

import Directories (globDirs, getStackSubdir, listCurrentDirectory,
                    traversePlatforms, traversePlatforms')
import qualified Remove
import Types
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

sizeSnapshots :: Bool -> Maybe String -> IO ()
sizeSnapshots nothuman msystem =
  traversePlatforms' (getStackSubdir "snapshots") msystem $ \plat ->
  cmd_ "du" $ ["-h" | not nothuman] ++ ["-s", plat]

listGhcSnapshots :: Maybe String -> Maybe Version -> FilePath -> IO ()
listGhcSnapshots msystem mghcver dir = do
  traversePlatforms (return dir) msystem $ do
    ghcs <- getSnapshotDirs mghcver
    mapM_ printTotalGhcSize ghcs

plural :: String -> Int -> String
plural thing n = show n ++ " " ++ thing ++ if n == 1 then "" else "s"

removeVersionSnaps :: Deletion -> FilePath -> VersionSnapshots -> IO ()
removeVersionSnaps deletion cwd versnap = do
  let dirs = snapsHashes versnap
  dir <- getCurrentDirectory
  home <- getHomeDirectory
  putStrLn $ plural "dir" (length dirs) ++ " in " ++ renderDir home dir ++ " " ++ (if isDelete deletion then "" else "would be ") ++ "removed for " ++ showVersion (snapsVersion versnap)
  mapM_ (Remove.doRemoveDirectory deletion) dirs
  where
    renderDir :: FilePath -> FilePath -> FilePath
    renderDir home fp =
      case stripPrefix (cwd ++ "/") fp of
        Just reldir -> reldir
        Nothing -> "~" </> dropPrefix (home ++ "/") fp

cleanGhcSnapshots :: Deletion -> FilePath -> Version -> IO ()
cleanGhcSnapshots deletion cwd ghcver = do
  ghcs <- getSnapshotDirs (Just ghcver)
  when (isMajorVersion ghcver) $ do
    Remove.prompt deletion ("all " ++ showVersion ghcver ++ " builds")
  mapM_ (removeVersionSnaps deletion cwd) ghcs

cleanMinorSnapshots :: Deletion -> FilePath -> Maybe Version -> IO ()
cleanMinorSnapshots deletion cwd mghcver = do
  ghcs <- getSnapshotDirs (majorVersion <$> mghcver)
  case mghcver of
    Nothing -> do
      let majors = groupOn (majorVersion . snapsVersion) ghcs
      forM_ majors $ \ gmajor ->
        when (length gmajor > 1) $
        mapM_ (removeVersionSnaps deletion cwd) (init gmajor)
    Just ghcver -> do
      let newestMinor = if isMajorVersion ghcver
                        then (snapsVersion . last) ghcs
                        else ghcver
          gminors = filter ((< newestMinor) . snapsVersion) ghcs
      mapM_ (removeVersionSnaps deletion cwd) gminors

cleanOldStackWork :: Deletion -> Natural -> Maybe String -> IO ()
cleanOldStackWork deletion keep msystem = do
  traversePlatforms (return stackWorkInstall) msystem $ do
    dirs <- sortOn takeFileName . lines <$> shell ( unwords $ "ls" : ["-d", "*/*"])
    let ghcs = sortOn (readVersion . takeFileName . head) $
               groupOn takeFileName dirs
    mapM_ removeOlder ghcs
  where
    removeOlder :: [FilePath] -> IO ()
    removeOlder dirs = do
      let ghcver = (takeFileName . head) dirs
      oldfiles <- drop (fromEnum keep) . reverse <$> sortedByAge
      mapM_ (Remove.doRemoveDirectory deletion . takeDirectory) oldfiles
      unless (null oldfiles) $
        putStrLn $ plural "dir" (length oldfiles) ++ (if isDelete deletion then "" else " would be") ++ " removed for " ++ ghcver
      where
        sortedByAge = do
          fileTimes <- mapM newestTimeStamp dirs
          return $ map fst $ sortOn snd fileTimes

        newestTimeStamp dir = do
          withCurrentDirectory dir $ do
            files <- listCurrentDirectory
            timestamp <- maximum <$> mapM getModificationTime files
            return (dir, timestamp)

stackWorkInstall :: FilePath
stackWorkInstall = ".stack-work/install"

sizeStackWork :: Bool -> FilePath -> IO ()
sizeStackWork nothuman dir = do
  let path = dir </> ".stack-work"
  whenM (doesDirectoryExist path) $
    cmd_ "du" $ ["-h" | not nothuman] ++ ["-s", path]

printTotalGhcSize :: VersionSnapshots -> IO ()
printTotalGhcSize versnaps = do
  total <- head . words . last <$> cmdLines "du" ("-shc":snapsHashes versnaps)
  printf "%4s  %-6s (%d dirs)\n" total ((showVersion . snapsVersion) versnaps) (length (snapsHashes versnaps))

removeStackWork :: Deletion -> IO ()
removeStackWork deletion = do
  Remove.prompt deletion ".stack-work"
  Remove.doRemoveDirectory deletion ".stack-work"
