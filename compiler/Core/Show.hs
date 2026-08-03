module Core.Show where

import Data.Kind (Constraint, Type)
import Semantic.Scope (Environment)

type Show :: (Type -> Environment -> Type) -> Constraint
class Show typef where
  showsPrec :: (Prelude.Show logical) => Int -> typef logical scope -> ShowS
