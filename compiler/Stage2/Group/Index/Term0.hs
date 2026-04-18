module Stage2.Group.Index.Term0 where

import qualified Stage2.Index.Term0 as Proper (Index)

data Index scope
  = Index !(Proper.Index scope)
  | Share !(Proper.Index scope)
  deriving (Show, Eq, Ord)
