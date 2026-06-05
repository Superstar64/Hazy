module Semantic.Connect where

import Data.Kind (Constraint, Type)
import Semantic.Layout (Group, Layout, Normal)
import Semantic.Scope (Environment)
import Semantic.Stage (Check, Resolve, Stage)

type Connect :: (Layout -> Stage -> Environment -> Type) -> Constraint
class Connect f where
  connect :: f Normal Resolve scope -> f Group Resolve scope
  seperate :: f Group Check scope -> f Normal Check scope
