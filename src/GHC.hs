module GHC (
  listGhcInstallation,
  removeGhcMinorInstallation,
  removeGhcVersionInstallation,
  sizeGhcPrograms
  )
where

import Control.Monad.Extra
import Data.Char (isDigit)
import Data.List.Extra
import Data.Version.Extra
import SimpleCmd
import System.FilePath

import Directories (getStackSubdir, globDirs, traversePlatforms)
import qualified Remove
import Types
import Versions

getStackProgramsDir :: IO FilePath
getStackProgramsDir =
  getStackSubdir "programs"

sizeGhcPrograms :: Bool -> IO ()
sizeGhcPrograms nothuman = do
  programs <- getStackProgramsDir
  cmd_ "du" $ ["-h" | not nothuman] ++ ["-s", programs]

getGhcInstallDirs :: Maybe Version -> IO [FilePath]
getGhcInstallDirs mghcver =
  sortOn ghcInstallVersion <$> globDirs ("ghc" ++ matchVersion)
  where
    matchVersion =
      case mghcver of
        Nothing -> "*"
        Just ver ->
          "*-" ++ showVersion ver ++ if isMajorVersion ver then "*" else ""

ghcInstallVersion :: FilePath -> Version
ghcInstallVersion =
  readVersion . checkChars . takeWhileEnd (/= '-') .  dropSuffix ".temp"
  where
    checkChars vs =
      let isVerChar c = isDigit c || c == '.'
      in if all isVerChar vs
         then vs
         else error $ "unknown version:" +-+ vs

listGhcInstallation :: Maybe Version -> Maybe String -> IO ()
listGhcInstallation mghcver msystem = do
  traversePlatforms getStackProgramsDir msystem $ do
    dirs <- getGhcInstallDirs mghcver
    mapM_ putStrLn $
      case mghcver of
        Nothing -> dirs
        Just ghcver -> filter ((== ghcver) . (if isMajorVersion ghcver then majorVersion else id) . ghcInstallVersion) dirs

removeGhcVersionInstallation :: Deletion -> Version -> Maybe String -> IO ()
removeGhcVersionInstallation deletion ghcver msystem = do
  traversePlatforms getStackProgramsDir msystem $ do
    installs <- getGhcInstallDirs (Just ghcver)
    case installs of
      [] -> putStrLn $ "stack ghc compiler version " ++ showVersion ghcver ++ " not found"
      [g] | not (isMajorVersion ghcver) -> doRemoveGhcVersion deletion g
      gs ->
        if isMajorVersion ghcver
        then do
          Remove.prompt deletion ("all stack ghc " ++ showVersion ghcver ++ " installations: ")
          mapM_ (doRemoveGhcVersion deletion) gs
        else error' "more than one match found!!"

removeGhcMinorInstallation :: Deletion -> Maybe Version -> Maybe String
                           -> IO ()
removeGhcMinorInstallation deletion mghcver msystem = do
  traversePlatforms getStackProgramsDir msystem $ do
    dirs <- getGhcInstallDirs (majorVersion <$> mghcver)
    unless (null dirs) $
      case mghcver of
        Nothing -> do
          let majors = groupOn (majorVersion . ghcInstallVersion) dirs
          forM_ majors $ \ minors ->
            forM_ (init minors) $ doRemoveGhcVersion deletion
        Just ghcver -> do
          let minors =
                if isMajorVersion ghcver
                then init dirs
                else filter ((< ghcver) . ghcInstallVersion) dirs
          forM_ minors $ doRemoveGhcVersion deletion

doRemoveGhcVersion :: Deletion -> FilePath -> IO ()
doRemoveGhcVersion deletion ghcinst = do
  Remove.doRemoveDirectory deletion ghcinst
  Remove.removeFile deletion (ghcinst <.> "installed")
  putStrLn $ ghcinst ++ " compiler " ++ (if isDelete deletion then "" else "would be ") ++ "removed"
