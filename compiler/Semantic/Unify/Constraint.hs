module Semantic.Unify.Constraint where

import Core.Tree.Constraint (ConstraintF (..))
import {-# SOURCE #-} Semantic.Unify.Type (Logical)

type Constraint s scope = ConstraintF (Logical s scope) scope
