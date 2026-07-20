{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Instanciation where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import {-# SOURCE #-} Core.Tree.Evidence (EvidenceF)
import qualified Data.Vector.Strict as Strict
import Semantic.Scope (Vacuous)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data InstanciationF logical scope
  = Instanciation !(Strict.Vector (EvidenceF logical scope))
  | Mono

instance (Scope.Show logical) => Show (InstanciationF logical scope)

instance (Shift.Functor logical) => Shift0.Functor (InstanciationF logical)

instance (Shift.Functor logical) => Shift.Functor (InstanciationF logical)

instance (logical ~ Vacuous) => Shift2.Functor (InstanciationF logical)

instance (logical ~ Vacuous) => Substitute.Functor (InstanciationF logical)

instance Substitute.EvidenceFunctor InstanciationF
