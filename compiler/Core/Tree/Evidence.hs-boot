{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Evidence where

import Data.Kind (Type)
import Data.Void (Void)
import Semantic.Scope (Environment)

type Evidence = EvidenceF Void

type role EvidenceF representational nominal

type EvidenceF :: Type -> Environment -> Type
data EvidenceF logical scope
