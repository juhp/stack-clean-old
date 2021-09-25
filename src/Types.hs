module Types (
  Deletion (..),
  isDelete
  )
where

data Deletion = Dryrun | Delete
  deriving Eq

isDelete :: Deletion -> Bool
isDelete = (== Delete)
