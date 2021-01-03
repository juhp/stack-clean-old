module Remove (
  doRemoveDirectory,
  removeFile
  )
where

import Control.Monad.Extra
import qualified System.Directory as D

doRemoveDirectory :: Bool -> FilePath -> IO ()
doRemoveDirectory dryrun dir =
  unless dryrun $
  D.removeDirectoryRecursive dir

removeFile :: Bool -> FilePath -> IO ()
removeFile dryrun file =
  unless dryrun $
  whenM (D.doesFileExist file) $
  D.removeFile file
