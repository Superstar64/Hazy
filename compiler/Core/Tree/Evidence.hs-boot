{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Evidence where

import Data.Kind (Type)
import Semantic.Scope (Environment, Vacuous)

type Evidence = EvidenceF Vacuous

type role EvidenceF representational nominal

type EvidenceF :: (Environment -> Type) -> Environment -> Type
data EvidenceF logical scope
