module Semantic.Shift0 where

import Data.Kind (Constraint, Type)
import Semantic.Scope (Environment)

type Functor :: (Environment -> Type) -> Constraint
class Functor functor
