module Stage2.Connect where

import Data.Kind (Constraint, Type)
import Stage2.Layout (Group, Layout, Normal)
import Stage2.Scope (Environment)
import Stage2.Stage (Check, Resolve, Stage)

type Connect :: (Layout -> Stage -> Environment -> Type) -> Constraint
class Connect f where
  connect :: f Normal Resolve scope -> f Group Resolve scope
  seperate :: f Group Check scope -> f Normal Check scope
