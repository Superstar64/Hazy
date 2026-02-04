module Stage3.Tree.InstanceMethod where

import Stage2.Scope (Environment (..), Local)
import Stage3.Tree.Definition (Definition)
import {-# SOURCE #-} qualified Stage4.Tree.Expression as Simple (Expression)

data InstanceMethod scope
  = Definition
      { constraintCount :: !Int,
        definition :: !(Definition (Local ':+ Local ':+ scope)),
        definition' :: !(Simple.Expression (Local ':+ Local ':+ scope))
      }
  | Default
      { constraintCount :: !Int,
        definition' :: !(Simple.Expression (Local ':+ Local ':+ scope))
      }
  deriving (Show)
