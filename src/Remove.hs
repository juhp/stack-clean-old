module Remove (
  doRemoveDirectory,
  prompt,
  removeFile
  )
where

import Control.Monad.Extra
import qualified System.Directory as D

import Types

doRemoveDirectory :: Deletion -> FilePath -> IO ()
doRemoveDirectory deletion dir =
  when (isDelete deletion) $
  D.removeDirectoryRecursive dir

removeFile :: Deletion -> FilePath -> IO ()
removeFile deletion file =
  when (isDelete deletion) $
  whenM (D.doesFileExist file) $
  D.removeFile file

prompt :: Deletion -> String -> IO ()
prompt deletion str =
  when (isDelete deletion) $ do
  putStr $ "Press Enter to delete " ++ str ++ ": "
  void getLine
