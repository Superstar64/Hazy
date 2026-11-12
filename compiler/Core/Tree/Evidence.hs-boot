{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Evidence where

import Data.Kind (Type)
import Data.Void (Void)
import Semantic.Scope (Environment)
import qualified Semantic.Shift0 as Shift0

type Evidence = EvidenceF Void Void

type role EvidenceF representational representational nominal

type EvidenceF :: Type -> Type -> Environment -> Type
data EvidenceF logicalEvidence logicalType scope

instance Shift0.Functor (EvidenceF logicalEvidence logicalType)
