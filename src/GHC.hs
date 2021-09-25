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
import Types
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

removeGhcVersionInstallation :: Deletion -> Version -> IO ()
removeGhcVersionInstallation deletion ghcver = do
  installs <- getGhcInstallDirs (Just ghcver)
  case installs of
    [] -> putStrLn $ "stack ghc compiler version " ++ showVersion ghcver ++ " not found"
    [g] | not (isMajorVersion ghcver) -> doRemoveGhcVersion deletion g
    gs -> if isMajorVersion ghcver then do
      Remove.prompt deletion ("all stack ghc " ++ showVersion ghcver ++ " installations: ")
      mapM_ (doRemoveGhcVersion deletion) gs
      else error' "more than one match found!!"

removeGhcMinorInstallation :: Deletion -> Maybe Version -> IO ()
removeGhcMinorInstallation deletion mghcver = do
  dirs <- getGhcInstallDirs (majorVersion <$> mghcver)
  case mghcver of
    Nothing -> do
      let majors = groupOn (majorVersion . ghcInstallVersion) dirs
      forM_ majors $ \ minors ->
        forM_ (init minors) $ doRemoveGhcVersion deletion
    Just ghcver -> do
      let minors = filter ((< ghcver) . ghcInstallVersion) dirs
      forM_ minors $ doRemoveGhcVersion deletion

doRemoveGhcVersion :: Deletion -> FilePath -> IO ()
doRemoveGhcVersion deletion ghcinst = do
  Remove.doRemoveDirectory deletion ghcinst
  Remove.removeFile deletion (ghcinst <.> "installed")
  putStrLn $ ghcinst ++ " compiler " ++ (if isDelete deletion then "" else "would be ") ++ "removed"
