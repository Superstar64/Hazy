module Stage3.Tree.MethodConcrete where

import Stage2.Scope (Environment (..), Local)
import Stage3.Tree.Definition (Definition)
import qualified Stage4.Tree.Evidence as Simple (Evidence)
import {-# SOURCE #-} qualified Stage4.Tree.Expression as Simple (Expression)
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver)
import qualified Stage4.Tree.Type as Simple (Type)

data MethodConcrete scope
  = Definition
      { definition :: !(Simple.SchemeOver Definition (Local ':+ scope))
      }
  | Default
      { base :: !(Simple.Type (Local ':+ scope)),
        self :: !(Simple.Evidence (Local ':+ scope)),
        defaultx :: !(Simple.SchemeOver Simple.Expression (Local ':+ scope))
      }
  deriving (Show)
