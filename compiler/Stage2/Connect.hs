module Stage2.Connect where

import Data.Kind (Constraint, Type)
import Stage2.Layout (Group, Layout, Normal)
import Stage2.Scope (Environment)

type Connect :: (Layout -> Environment -> Type) -> Constraint
class Connect f where
  connect :: f Normal scope -> f Group scope
