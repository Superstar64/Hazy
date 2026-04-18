module Stage2.Group.Temporary.Declaration where

import qualified Stage2.Tree.Declaration as Proper (Declaration)
import qualified Stage2.Tree.Shared as Proper (Shared)

data Declaration scope
  = Declaration !(Proper.Declaration scope)
  | Shared !(Proper.Shared scope)
