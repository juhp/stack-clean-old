module GHC (
  listGhcInstallation,
  removeGhcMinorInstallation,
  removeGhcVersionInstallation,
  sizeGhcInstalls
  )
where

import Control.Monad.Extra
import Data.List.Extra
import Data.Version.Extra
import SimpleCmd
import System.FilePath

import Directories (getStackSubdir, globDirs, switchToSystemDirUnder)
import qualified Remove
import Versions

getStackProgramsDir :: IO FilePath
getStackProgramsDir =
  getStackSubdir "programs"

sizeGhcInstalls :: Bool -> IO ()
sizeGhcInstalls nothuman = do
  programs <- getStackProgramsDir
  cmd_ "du" $ ["-h" | not nothuman] ++ ["-s", programs]

setStackProgramsDir :: IO ()
setStackProgramsDir =
  getStackProgramsDir >>= switchToSystemDirUnder

getGhcInstallDirs :: Maybe Version -> IO [FilePath]
getGhcInstallDirs mghcver = do
  setStackProgramsDir
  sortOn ghcInstallVersion <$> globDirs matchVersion
  where
    matchVersion =
      case mghcver of
        Nothing -> "*"
        Just ver ->
          "*-" ++ showVersion ver ++ if isMajorVersion ver then "*" else ""

ghcInstallVersion :: FilePath -> Version
ghcInstallVersion =
  readVersion . takeWhileEnd (/= '-') .  dropSuffix ".temp"

listGhcInstallation :: Maybe Version -> IO ()
listGhcInstallation mghcver = do
  dirs <- getGhcInstallDirs mghcver
  mapM_ putStrLn $ case mghcver of
    Nothing -> dirs
    Just ghcver -> filter ((== ghcver) . (if isMajorVersion ghcver then majorVersion else id) . ghcInstallVersion) dirs

removeGhcVersionInstallation :: Bool -> Version -> IO ()
removeGhcVersionInstallation dryrun ghcver = do
  installs <- getGhcInstallDirs (Just ghcver)
  case installs of
    [] -> error' $ "stack ghc compiler version " ++ showVersion ghcver ++ " not found"
    [g] | not (isMajorVersion ghcver) -> doRemoveGhcVersion dryrun g
    gs -> if isMajorVersion ghcver then do
      putStr $ "Press Enter to delete all stack " ++ showVersion ghcver ++ " installations: "
      void getLine
      mapM_ (doRemoveGhcVersion dryrun) gs
      else error' "more than one match found!!"

removeGhcMinorInstallation :: Bool -> Maybe Version -> IO ()
removeGhcMinorInstallation dryrun mghcver = do
  dirs <- getGhcInstallDirs (majorVersion <$> mghcver)
  case mghcver of
    Nothing -> do
      let majors = groupOn (majorVersion . ghcInstallVersion) dirs
      forM_ majors $ \ minors ->
        forM_ (init minors) $ doRemoveGhcVersion dryrun
    Just ghcver -> do
      let minors = filter ((< ghcver) . ghcInstallVersion) dirs
      forM_ minors $ doRemoveGhcVersion dryrun

doRemoveGhcVersion :: Bool -> FilePath -> IO ()
doRemoveGhcVersion dryrun ghcinst = do
  Remove.doRemoveDirectory dryrun ghcinst
  Remove.removeFile dryrun (ghcinst <.> "installed")
  putStrLn $ ghcinst ++ " removed"
