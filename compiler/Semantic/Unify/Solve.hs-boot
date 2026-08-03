module Semantic.Unify.Solve where

import Control.Monad.ST (ST)
import Core.Tree.Constraint (ConstraintF)
import Core.Tree.Constraints (ConstraintsF)
import Core.Tree.Evidence (EvidenceF)
import Core.Tree.Forall (ForallOver)
import Core.Tree.Instanciation (InstanciationF)
import Core.Tree.Type (TypeF)
import Data.Kind (Constraint)
import qualified Data.Kind as Kind
import Data.Void (Void)
import Semantic.Scope (Environment)
import qualified Semantic.Shift0 as Shift0
import {-# SOURCE #-} qualified Semantic.Unify.Evidence as Evidence (Logical)
import {-# SOURCE #-} Semantic.Unify.Type (Logical)
import Syntax.Position (Position)

newtype Solve s a = Solve (ST s a)

instance Functor (Solve s)

instance Applicative (Solve s)

instance Monad (Solve s)

type SolveType :: (Kind.Type -> Environment -> Kind.Type) -> Constraint
class SolveType typef where
  solveWith ::
    Shift0.Category scopex scope ->
    Position ->
    typef (Logical s scopex) scope ->
    Solve s (typef Void scope)

solve ::
  (SolveType typef) =>
  Position ->
  typef (Logical s scope) scope ->
  Solve s (typef Void scope)

instance SolveType TypeF

instance SolveType ConstraintF

instance SolveType ConstraintsF

instance (SolveType typef) => SolveType (ForallOver typef)

type SolveEvidence :: (Kind.Type -> Environment -> Kind.Type) -> Constraint
class SolveEvidence evidencef where
  solveEvidenceWith ::
    Shift0.Category scopex scope ->
    Position ->
    evidencef (Evidence.Logical s scopex) scope ->
    Solve s (evidencef Void scope)

solveEvidence ::
  (SolveEvidence evidencef) =>
  Position ->
  evidencef (Evidence.Logical s scope) scope ->
  Solve s (evidencef Void scope)

instance SolveEvidence EvidenceF

instance SolveEvidence InstanciationF

runSolve :: Solve s a -> ST s a
liftST :: ST s a -> Solve s a
