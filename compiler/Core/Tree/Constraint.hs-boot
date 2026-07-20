{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Constraint where

import Data.Kind (Type)
import Semantic.Scope (Environment, Vacuous)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import qualified Semantic.Tree.Constraint as Solved

type Constraint = ConstraintF Vacuous

type role ConstraintF representational nominal

type ConstraintF :: (Environment -> Type) -> Environment -> Type
data ConstraintF logical scope

instance (Scope.Show logical) => Show (ConstraintF logical scope)

instance (Shift.Functor logical) => Shift0.Functor (ConstraintF logical)

instance (Shift.Functor logical) => Shift.Functor (ConstraintF logical)

simplify :: Solved.Constraint position Check scope -> Constraint scope
