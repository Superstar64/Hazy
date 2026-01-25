module Stage2.Shift where

import Data.Kind (Constraint, Type)
import Stage2.Scope (Environment)

type Functor :: (Environment -> Type) -> Constraint
class Functor functor
