module Versions (
  isMajorVersion,
  majorVersion
  )
where

import Data.Version
import SimpleCmd ((+-+))

majorVersion :: Version -> Version
majorVersion ver =
  case versionBranch ver of
    [_,_] -> ver
    (v1:v2:_) -> makeVersion [v1,v2]
    _ -> error $ "Bad ghc version" +-+ showVersion ver

isMajorVersion :: Version -> Bool
isMajorVersion ver =
  majorVersion ver == ver
