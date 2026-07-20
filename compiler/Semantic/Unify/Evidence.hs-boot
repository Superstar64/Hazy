{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Evidence where

import Control.Monad.ST (ST)
import Core.Tree.Evidence (EvidenceF)
import {-# SOURCE #-} qualified Core.Tree.Evidence as Solved (Evidence)
import qualified Data.Kind as Kind
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Unify.Class (Solve, Zonk)
import Syntax.Position (Position)

newtype Evidence s scope = Evidencex {runEvidencex :: EvidenceF (Logical s) scope}

instance Shift0.Functor (Evidence s)

instance Zonk Evidence

type role Logical nominal nominal

type Logical :: Kind.Type -> Environment -> Kind.Type
data Logical s scope

instance Shift0.Functor (Logical s)

instance Shift.Functor (Logical s)

unify :: EvidenceF (Logical s) scope -> EvidenceF (Logical s) scope -> ST s ()
unshift :: EvidenceF (Logical s) (scope ':+ scopes) -> ST s (EvidenceF (Logical s) scopes)
solve :: Position -> EvidenceF (Logical s) scope -> Solve s (Solved.Evidence scope)
