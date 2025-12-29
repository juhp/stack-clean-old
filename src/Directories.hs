{-# LANGUAGE CPP #-}

module Directories (
  getStackSubdir,
  getStackProgramsDir,
  globDirs,
  traversePlatforms,
  traversePlatforms',
  listCurrentDirectory
  )
where

import Control.Monad (filterM, forM_, unless, when)
import Data.List.Extra
import Safe (headMay)
import SimpleCmd ((+-+),
#if MIN_VERSION_simple_cmd(0,2,0)
                  warning
#endif
                 )
import System.Directory
import System.Environment
import System.FilePath
import System.FilePath.Glob
#if !MIN_VERSION_simple_cmd(0,2,0)
-- for warning
import System.IO (hPutStrLn, stderr)
#endif

globDirs :: String -> IO [FilePath]
globDirs pat = do
  map (dropPrefix "./") <$> namesMatching (pat ++ "/")

getStackRootDir :: IO FilePath
getStackRootDir = do
  mroot <- lookupEnv "STACK_ROOT"
  case mroot of
    Just path -> do
      unless (isAbsolute path) $
        warning $ "STACK_ROOT is not absolute:" +-+ path
      return path
    Nothing -> do
      home <- getHomeDirectory
      return $ home </> ".stack"

getStackProgramsDir :: IO FilePath
getStackProgramsDir =
  getStackSubdir "programs"

getStackSubdir :: FilePath -> IO FilePath
getStackSubdir subdir = do
  stackRoot <- getStackRootDir
  return $ stackRoot </> subdir

traversePlatforms :: IO FilePath -> Maybe String -> IO () -> IO ()
traversePlatforms getdir msystem act = do
  dir <- getdir
  withCurrentDirectory dir $ do
    platforms <- listPlatforms msystem
    forM_ platforms $ \p -> do
      when (length platforms > 1) $
        putStrLn (p ++ ":")
      withCurrentDirectory p act

traversePlatforms' :: IO FilePath -> Maybe String -> (FilePath -> IO ())
                   -> IO ()
traversePlatforms' getdir msystem act = do
  dir <- getdir
  withCurrentDirectory dir $ do
    platforms <- listPlatforms msystem
    mapM_ act platforms

listPlatforms :: Maybe String -> IO [FilePath]
listPlatforms msystem = do
  platforms <- listDirectory "." >>= filterM doesDirectoryExist
  case msystem of
    Nothing -> return platforms
    Just s ->
      if s `elem` platforms
      then return [s]
      else do
        warning $ "no matching platform for:" +-+ s
        return []

listCurrentDirectory :: IO [FilePath]
listCurrentDirectory =
  listDirectory "." >>= filterM doesDirectoryExist . filter (\d -> headMay d /= Just '.')

#if !MIN_VERSION_simple_cmd(0,2,0)
warning :: String -> IO ()
warning = hPutStrLn stderr
#endif
