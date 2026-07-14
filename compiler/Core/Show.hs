module Core.Show where

import Data.Kind (Constraint, Type)
import Semantic.Scope (Environment)
import qualified Semantic.Scope as Scope

type Show :: ((Environment -> Type) -> Environment -> Type) -> Constraint
class Show typef where
  showsPrec :: (Scope.Show logical) => Int -> typef logical scope -> ShowS
