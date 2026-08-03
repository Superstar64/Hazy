{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Constraint where

import Data.Kind (Type)
import Data.Void (Void)
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import qualified Semantic.Tree.Constraint as Solved

type Constraint = ConstraintF Void

type role ConstraintF representational nominal

type ConstraintF :: Type -> Environment -> Type
data ConstraintF logical scope

instance (Show logical) => Show (ConstraintF logical scope)

instance Shift0.Functor (ConstraintF logical)

instance Shift.Functor (ConstraintF logical)

simplify :: Solved.Constraint position Check scope -> Constraint scope
