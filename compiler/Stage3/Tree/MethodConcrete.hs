module Stage3.Tree.MethodConcrete where

import qualified Data.Vector.Strict as Strict
import Stage2.Scope (Environment (..), Local)
import Stage3.Tree.Definition (Definition)
import qualified Stage4.Tree.Constraint as Simple (Constraint)
import qualified Stage4.Tree.Evidence as Simple (Evidence)
import {-# SOURCE #-} qualified Stage4.Tree.Expression as Simple (Expression)
import qualified Stage4.Tree.Type as Simple (Type)

data MethodConcrete scope
  = Definition
      { parameters :: !(Strict.Vector (Simple.Type (Local ':+ scope))),
        constraints :: !(Strict.Vector (Simple.Constraint (Local ':+ scope))),
        definition :: !(Definition (Local ':+ Local ':+ scope))
      }
  | Default
      { parameters :: !(Strict.Vector (Simple.Type (Local ':+ scope))),
        constraints :: !(Strict.Vector (Simple.Constraint (Local ':+ scope))),
        base :: !(Simple.Type (Local ':+ scope)),
        self :: !(Simple.Evidence (Local ':+ scope)),
        defaultx :: !(Simple.Expression (Local ':+ (Local ':+ scope)))
      }
  deriving (Show)
