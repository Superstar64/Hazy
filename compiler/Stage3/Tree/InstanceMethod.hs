module Stage3.Tree.InstanceMethod where

import Stage2.Scope (Environment (..), Local)
import Stage3.Tree.Definition (Definition)
import {-# SOURCE #-} qualified Stage4.Tree.Expression as Simple (Expression)
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver)

data InstanceMethod scope
  = Definition
      { definition :: !(Definition (Local ':+ Local ':+ scope)),
        definition' :: !(Simple.SchemeOver Simple.Expression (Local ':+ scope))
      }
  | Default
      { definition' :: !(Simple.SchemeOver Simple.Expression (Local ':+ scope))
      }
  deriving (Show)
