{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Constraints where

import Core.Tree.Constraints (ConstraintsF)
import {-# SOURCE #-} Semantic.Unify.Type (Logical)

newtype Constraints s scope = Constraintsx {runConstraintsx :: ConstraintsF (Logical s) scope}
