module Stage4.Tree.Instance where

import qualified Data.Vector.Strict as Strict
import Stage2.Scope (Environment (..), Local)
import Stage4.Tree.Evidence (Evidence)
import {-# SOURCE #-} Stage4.Tree.Expression (Expression)

data Instance scope = Instance
  { evidence :: !(Strict.Vector (Evidence (Local ':+ scope))),
    prerequisitesCount :: !Int,
    memberConstraintCounts :: !(Strict.Vector Int),
    members :: !(Strict.Vector (Expression (Local ':+ Local ':+ scope)))
  }
  deriving (Show)
