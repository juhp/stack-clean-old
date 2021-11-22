module Directories (
  getStackSubdir,
  globDirs,
  switchToSystemDirUnder
  )
where

import Data.List.Extra
import SimpleCmd (error')
import System.Directory
import System.FilePath
import System.FilePath.Glob

globDirs :: String -> IO [FilePath]
globDirs pat = do
  map (dropPrefix "./") <$> namesMatching (pat ++ "/")

getStackSubdir :: FilePath -> IO FilePath
getStackSubdir subdir = do
  home <- getHomeDirectory
  return $ home </> ".stack" </> subdir

switchToSystemDirUnder :: Maybe String -> FilePath -> IO ()
switchToSystemDirUnder msystem dir = do
  exists <- doesDirectoryExist dir
  if exists
    then setCurrentDirectory dir
    else error' $ dir ++ " not found"
  systems <- listDirectory "."
  -- FIXME be more precise/check "system" dirs
  -- eg 64bit intel Linux: x86_64-linux-tinfo6
  let system =
        case msystem of
          Just sys ->
            if sys `elem` systems
            then sys
            else error' $ sys ++ " not found"
          Nothing ->
            case systems of
              [] -> error' $ "No OS system in " ++ dir
              [sys] -> sys
              ss -> error' $ intercalate "\n" $
                    ["Please specify platform with --os-system (-o).",
                      dir ++ " has:"] ++ ss
  setCurrentDirectory system
