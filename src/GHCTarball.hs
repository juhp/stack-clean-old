module GHCTarball (
  listGhcTarballs,
  removeGhcMinorTarball,
  removeGhcVersionTarball
  )
where

import Control.Monad.Extra
import Data.Char (isDigit)
import Data.List.Extra
import Data.Version.Extra
import SimpleCmd
import System.FilePath (dropExtension)
import System.FilePath.Glob

import Directories (getStackSubdir, switchToSystemDirUnder)
import qualified Remove
import Types
import Versions

getStackProgramsDir :: IO FilePath
getStackProgramsDir =
  getStackSubdir "programs"

setStackProgramsDir :: Maybe String -> IO ()
setStackProgramsDir msystem =
  getStackProgramsDir >>= switchToSystemDirUnder msystem

getGhcTarballs :: Maybe Version -> Maybe String -> IO [FilePath]
getGhcTarballs mghcver msystem = do
  setStackProgramsDir msystem
  sortOn ghcTarballVersion . map (dropPrefix "./") <$> namesMatching ("ghc" ++ matchVersion ++ ".tar.*")
  where
    matchVersion =
      case mghcver of
        Nothing -> "*"
        Just ver ->
          "*-" ++ showVersion ver ++ if isMajorVersion ver then "*" else ""

ghcTarballVersion :: FilePath -> Version
ghcTarballVersion =
  readVersion . checkChars . takeWhileEnd (/= '-') .  dropSuffix ".tar" . dropExtension
  where
    checkChars vs =
      let isVerChar c = isDigit c || c == '.'
      in if all isVerChar vs
         then vs
         else error $ "unknown version:" +-+ vs

listGhcTarballs :: Maybe Version -> Maybe String -> IO ()
listGhcTarballs mghcver msystem = do
  files <- getGhcTarballs mghcver msystem
  mapM_ putStrLn $
    case mghcver of
      Nothing -> files
      Just ghcver -> filter ((== ghcver) . (if isMajorVersion ghcver then majorVersion else id) . ghcTarballVersion) files

removeGhcVersionTarball :: Deletion -> Version -> Maybe String -> IO ()
removeGhcVersionTarball deletion ghcver msystem = do
  files <- getGhcTarballs (Just ghcver) msystem
  case files of
    [] -> putStrLn $ "Tarball for " ++ showVersion ghcver ++ " not found"
    [g] | not (isMajorVersion ghcver) -> doRemoveGhcTarballVersion deletion g
    gs -> if isMajorVersion ghcver then do
      Remove.prompt deletion ("all stack ghc " ++ showVersion ghcver ++ " tarballs: ")
      mapM_ (doRemoveGhcTarballVersion deletion) gs
      else error' "more than one match found!!"

removeGhcMinorTarball :: Deletion -> Maybe Version -> Maybe String
                           -> IO ()
removeGhcMinorTarball deletion mghcver msystem = do
  files <- getGhcTarballs (majorVersion <$> mghcver) msystem
  case mghcver of
    Nothing -> do
      let majors = groupOn (majorVersion . ghcTarballVersion) files
      forM_ majors $ \ minors ->
        forM_ (init minors) $ doRemoveGhcTarballVersion deletion
    Just ghcver -> do
      let minors = filter ((< ghcver) . ghcTarballVersion) files
      forM_ minors $ doRemoveGhcTarballVersion deletion

doRemoveGhcTarballVersion :: Deletion -> FilePath -> IO ()
doRemoveGhcTarballVersion deletion ghctarball = do
  Remove.removeFile deletion ghctarball
  putStrLn $ ghctarball ++ " tarball " ++ (if isDelete deletion then "" else "would be ") ++ "removed"
