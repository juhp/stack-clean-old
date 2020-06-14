import Control.Monad.Extra
import Data.List.Extra
import SimpleCmd
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

-- number of builds to keep per ghc version
keep_dirs :: Int
keep_dirs = 4

cleanSnapshots :: IO ()
cleanSnapshots = do
  error' "write-me"

cleanStackWork :: Maybe FilePath -> IO ()
cleanStackWork mdir = do
  whenJust mdir $ \ dir -> setCurrentDirectory dir
  unlessM (doesDirectoryExist ".stack-work") $
    error' "No .stack-work/ found"
  setCurrentDirectory ".stack-work/install/"
  systems <- listDirectory "."
  let system = case systems of
        [] -> error' "Did not find any OS systems in .stack-work/install/"
        [sys] -> sys
        _ -> error' "More than one OS systems found (unsupported)"
  setCurrentDirectory system
  -- sort and then group by ghc version
  dirs <- sortOn takeFileName . lines <$> shell ( unwords $ "ls" : ["-d", "*/*"])
  let ghcs = groupOn takeFileName dirs
  mapM_ removeOlder ghcs
  where
    removeOlder dirs = do
      oldfiles <- drop keep_dirs . reverse <$> sortByAge dirs
      mapM_ (removeDirectoryRecursive . takeDirectory) oldfiles

    sortByAge dirs = do
      fileTimes <- mapM newestTimeStamp dirs
      return $ map fst $ sortOn snd fileTimes

    newestTimeStamp dir = do
      withCurrentDirectory dir $ do
        files <- listDirectory "."
        timestamp <- last . sort <$> mapM getModificationTime files
        return (dir, timestamp)
