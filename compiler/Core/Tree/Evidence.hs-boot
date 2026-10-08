{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Evidence where

import Data.Kind (Type)
import Data.Void (Void)
import Semantic.Scope (Environment)

type Evidence = EvidenceF Void Void

type role EvidenceF representational representational nominal

type EvidenceF :: Type -> Type -> Environment -> Type
data EvidenceF logicalEvidence logicalType scope
