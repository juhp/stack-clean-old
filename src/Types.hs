module Types (
  Deletion (..),
  isDelete,
  deletePrompt
  )
where

data Deletion = Dryrun | Delete Bool

isDelete :: Deletion -> Bool
isDelete (Delete _) = True
isDelete Dryrun = False

deletePrompt :: Deletion -> Bool
deletePrompt (Delete False) = True
deletePrompt _ = False
