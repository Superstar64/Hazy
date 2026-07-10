{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Constraint where

import Core.Tree.Constraint (ConstraintF)
import {-# SOURCE #-} Semantic.Unify.Type (Logical)

newtype Constraint s scope = Constraintx {runConstraintx :: ConstraintF (Logical s) scope}
