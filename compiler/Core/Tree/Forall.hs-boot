{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Forall where

import qualified Core.Show as Core
import {-# SOURCE #-} Core.Tree.Type (TypeF)
import Data.Kind (Type)
import Semantic.Scope (Environment, Vacuous)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import qualified Semantic.Tree.Scheme as Solved

type Forall = ForallOver TypeF Vacuous

type role ForallOver representational nominal nominal

type ForallOver :: ((Environment -> Type) -> Environment -> Type) -> (Environment -> Type) -> Environment -> Type
data ForallOver typef logical scope

instance (Scope.Show logical, Core.Show typef) => Scope.Show (ForallOver typef logical)

instance (Shift.Functor logical, Shift.Functor (typef logical)) => Shift0.Functor (ForallOver typef logical)

simplify :: Solved.Scheme position Check scope -> Forall scope
