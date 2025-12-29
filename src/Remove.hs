module Remove (
  doRemoveDirectory,
  removeFile,
  wouldBeRemoved
  )
where

import Control.Monad.Extra
import SimpleCmd ((+-+))
import qualified System.Directory as D

import Types

doRemoveDirectory :: Deletion -> FilePath -> IO ()
doRemoveDirectory deletion dir = do
  when (isDelete deletion) $
    D.removeDirectoryRecursive dir
  putStrLn $ dir +-+ wouldBeRemoved deletion

removeFile :: Deletion -> FilePath -> IO ()
removeFile deletion file =
  when (isDelete deletion) $
  whenM (D.doesFileExist file) $
  D.removeFile file

wouldBeRemoved :: Deletion -> String
wouldBeRemoved deletion =
  (if isDelete deletion then "" else "would be") +-+ "removed"
