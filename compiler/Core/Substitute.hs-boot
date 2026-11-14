module Core.Substitute where

import Data.Kind (Constraint, Type)
import Semantic.Scope (Environment)

type Functor :: (Environment -> Type) -> Constraint
class Functor typex
