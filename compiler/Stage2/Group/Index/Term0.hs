module Stage2.Group.Index.Term0 where

import qualified Stage2.Index.Term0 as Single (Index)

data Index scope
  = Index !(Single.Index scope)
  | Share {shareIndex :: !Int}
  deriving (Show)
