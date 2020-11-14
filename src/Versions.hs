module Versions (
  isMajorVersion,
  majorVersion
  )
where

import Data.Version

majorVersion :: Version -> Version
majorVersion ver =
  let vernums = versionBranch ver in
    case length vernums of
      2 -> ver
      3 -> (makeVersion . init) vernums
      _ -> error $ "Bad ghc version " ++ showVersion ver

isMajorVersion :: Version -> Bool
isMajorVersion ver =
  majorVersion ver == ver
