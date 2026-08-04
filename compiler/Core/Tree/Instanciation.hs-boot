{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Instanciation where

import qualified Core.Substitute as Substitute
import {-# SOURCE #-} Core.Tree.Evidence (EvidenceF)
import qualified Core.Type.Functor as Core
import qualified Data.Vector.Strict as Strict
import Data.Void (Void)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data InstanciationF logical scope
  = Instanciation !(Strict.Vector (EvidenceF logical scope))
  | Mono

instance (Show logical) => Show (InstanciationF logical scope)

instance Shift0.Functor (InstanciationF logical)

instance Shift.Functor (InstanciationF logical)

instance (logical ~ Void) => Substitute.Functor (InstanciationF logical)

instance Substitute.EvidenceFunctor InstanciationF

instance Core.Functor InstanciationF
