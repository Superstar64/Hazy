{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Type where

import qualified Data.Kind
import Semantic.Scope (Environment, Vacuous)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import {-# SOURCE #-} qualified Semantic.Tree.Type as Semantic

type Type = TypeF Vacuous

type TypeF :: (Environment -> Data.Kind.Type) -> Environment -> Data.Kind.Type

type role TypeF representational nominal

data TypeF logical scope

instance (Scope.Eq logical) => Eq (TypeF logical scope)

instance (Scope.Show logical) => Show (TypeF logical scope)

instance (Shift.Functor logical) => Shift0.Functor (TypeF logical)

instance (Shift.Functor logical) => Shift.Functor (TypeF logical)

instance (Scope.Show logical) => Scope.Show (TypeF logical)

typex :: TypeF logical scope
simplify :: Semantic.Type position Check scope -> Type scope
