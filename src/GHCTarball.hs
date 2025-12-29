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
import SimplePrompt (yesNo)
import System.FilePath (dropExtension)
import System.FilePath.Glob

import Directories (getStackProgramsDir, traversePlatforms)
import qualified Remove
import Types
import Versions

-- setStackProgramsDir :: Maybe String -> IO ()
-- setStackProgramsDir msystem =
--   getStackProgramsDir >>= switchToSystemDirUnder msystem

getGhcTarballs :: Maybe Version -> IO [FilePath]
getGhcTarballs mghcver = do
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
  traversePlatforms getStackProgramsDir msystem $ do
    files <- getGhcTarballs mghcver
    mapM_ putStrLn $
      case mghcver of
        Nothing -> files
        Just ghcver -> filter ((== ghcver) . (if isMajorVersion ghcver then majorVersion else id) . ghcTarballVersion) files

removeGhcVersionTarball :: Deletion -> Version -> Maybe String -> IO ()
removeGhcVersionTarball deletion ghcver msystem = do
  traversePlatforms getStackProgramsDir msystem $ do
    files <- getGhcTarballs (Just ghcver)
    case files of
      [] -> putStrLn $ "Tarball for" +-+ showVersion ghcver +-+ "not found"
      [g] | not (isMajorVersion ghcver) -> doRemoveGhcTarballVersion deletion g
      gs ->
        if isMajorVersion ghcver
        then do
          yes <-
            if deletePrompt deletion
            then yesNo $ "Delete all stack ghc" +-+ showVersion ghcver +-+ "tarballs"
            else return True
          when yes $
            mapM_ (doRemoveGhcTarballVersion deletion) gs
        else error' "more than one match found!!"

removeGhcMinorTarball :: Deletion -> Maybe Version -> Maybe String
                           -> IO ()
removeGhcMinorTarball deletion mghcver msystem = do
  traversePlatforms getStackProgramsDir msystem $ do
    files <- getGhcTarballs (majorVersion <$> mghcver)
    unless (null files) $
      case mghcver of
        Nothing -> do
          let majors = groupOn (majorVersion . ghcTarballVersion) files
          forM_ majors $ \ minors ->
            forM_ (init minors) $ doRemoveGhcTarballVersion deletion
        Just ghcver -> do
          let minors =
                if isMajorVersion ghcver
                then init files
                else filter ((< ghcver) . ghcTarballVersion) files
          forM_ minors $ doRemoveGhcTarballVersion deletion

doRemoveGhcTarballVersion :: Deletion -> FilePath -> IO ()
doRemoveGhcTarballVersion deletion ghctarball = do
  Remove.removeFile deletion ghctarball
  putStrLn $ ghctarball +-+ "tarball" +-+ Remove.wouldBeRemoved deletion
