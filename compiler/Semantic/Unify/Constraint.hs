module Semantic.Unify.Constraint where

import Core.Tree.Constraint (ConstraintF (..))
import {-# SOURCE #-} Semantic.Unify.Type (Logical)
import Prelude hiding (Functor, map)

type Constraint s = ConstraintF (Logical s)
