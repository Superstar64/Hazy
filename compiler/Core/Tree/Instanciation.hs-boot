{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Instanciation where

import qualified Core.Evidence.Functor as Core
import qualified Core.Substitute as Substitute
import {-# SOURCE #-} Core.Tree.Evidence (EvidenceF)
import qualified Data.Vector.Strict as Strict
import Data.Void (Void)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data InstanciationF logicalEvidence logicalType scope
  = Instanciation !(Strict.Vector (EvidenceF logicalEvidence logicalType scope))
  | Mono

instance (Show logicalEvidence, Show logicalType) => Show (InstanciationF logicalEvidence logicalType scope)

instance Shift0.Functor (InstanciationF logicalEvidence logicalType)

instance Shift.Functor (InstanciationF logicalEvidence logicalType)

instance (logicalEvidence ~ Void, logicalType ~ Void) => Substitute.Functor (InstanciationF logicalEvidence logicalType)

instance Substitute.EvidenceFunctor InstanciationF

instance Core.Functor InstanciationF
