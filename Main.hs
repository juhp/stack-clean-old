{-# LANGUAGE CPP #-}

import Control.Monad.Extra
import Data.List.Extra
import SimpleCmd
#if MIN_VERSION_simple_cmd(0,2,1)
  hiding (ifM)
#endif
import SimpleCmdArgs
import System.Directory
import System.FilePath

import Paths_stack_clean_old (version)

main :: IO ()
main = do
  simpleCmdArgs (Just version) "Stack clean up tool"
    "Cleans away old stack snapshots and stack-work builds to recover diskspace." $
    subcommands
    [ Subcommand "snapshots" "purge older ~/.stack/snapshots" $
      pure cleanSnapshots
    , Subcommand "project" "purge older builds in .stack-work/install" $
      cleanStackWork <$> optional (strArg "PROJECTDIR")
    ]

cleanStackWork :: Maybe FilePath -> IO ()
cleanStackWork mdir = do
  whenJust mdir $ \ dir -> setCurrentDirectory dir
  switchToSystemDirUnder ".stack-work/install"
  -- number of builds to keep per ghc version
  cleanAwayOldBuilds 4

cleanSnapshots :: IO ()
cleanSnapshots = do
  home <- getHomeDirectory
  switchToSystemDirUnder $ home </> ".stack/snapshots"
  -- number of snapshots to keep per ghc version
  cleanAwayOldBuilds 50

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

cleanAwayOldBuilds :: Int -> IO ()
cleanAwayOldBuilds keep = do
  -- sort and then group by ghc version
  dirs <- sortOn takeFileName . lines <$> shell ( unwords $ "ls" : ["-d", "*/*"])
  let ghcs = groupOn takeFileName dirs
  mapM_ removeOlder ghcs
  where
    removeOlder :: [FilePath] -> IO ()
    removeOlder dirs = do
      oldfiles <- drop keep . reverse <$> sortedByAge
      mapM_ (removeDirectoryRecursive . takeDirectory) oldfiles
      where
        sortedByAge = do
          fileTimes <- mapM newestTimeStamp dirs
          return $ map fst $ sortOn snd fileTimes

        newestTimeStamp dir = do
          withCurrentDirectory dir $ do
            files <- listDirectory "."
            timestamp <- maximum <$> mapM getModificationTime files
            return (dir, timestamp)
