module Stage3.Unify.Class where

import qualified Data.Kind as Kind
import Stage2.Scope (Environment)

type Zonk :: (Kind.Type -> Environment -> Kind.Type) -> Kind.Constraint
class Zonk typex

type Functor :: (Environment -> Kind.Type) -> Kind.Constraint
class Functor typex
