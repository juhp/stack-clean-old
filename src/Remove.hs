module Remove (
  doRemoveDirectory,
  removeFile
  )
where

import Control.Monad
import System.Directory

doRemoveDirectory :: Bool -> FilePath -> IO ()
doRemoveDirectory dryrun dir =
  unless dryrun $
  removeDirectoryRecursive dir
