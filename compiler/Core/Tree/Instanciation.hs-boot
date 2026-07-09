{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Instanciation where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import {-# SOURCE #-} Core.Tree.Evidence (EvidenceF)
import qualified Data.Vector.Strict as Strict
import Semantic.Scope (IsVacuous)
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift)
import qualified Semantic.Shift as Shift

data InstanciationF logical scope
  = Instanciation !(Strict.Vector (EvidenceF logical scope))
  | Mono

instance (Scope.Show logical) => Show (InstanciationF logical scope)

instance (Shift.Functor logical) => Shift (InstanciationF logical)

instance (Shift.Functor logical) => Shift.Functor (InstanciationF logical)

instance (IsVacuous logical, Shift.Functor logical) => Shift2.Functor (InstanciationF logical)

instance (IsVacuous logical, Shift.Functor logical) => Substitute.Functor (InstanciationF logical)
