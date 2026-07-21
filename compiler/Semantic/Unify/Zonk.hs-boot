module Semantic.Unify.Zonk where

import Control.Monad.ST (ST)
import Core.Tree.Type (TypeF)
import qualified Data.Kind as Kind
import Semantic.Scope (Environment)
import {-# SOURCE #-} Semantic.Unify.Type (Logical)

data Zonker s s' where
  Zonker :: Zonker s s

instance Zonk TypeF

type Zonk :: ((Environment -> Kind.Type) -> Environment -> Kind.Type) -> Kind.Constraint
class Zonk typef where
  zonk :: Zonker s s' -> typef (Logical s) scope -> ST s (typef (Logical s') scope)
