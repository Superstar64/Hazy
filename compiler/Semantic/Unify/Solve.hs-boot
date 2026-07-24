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
import Semantic.Scope (Environment, Vacuous)
import {-# SOURCE #-} qualified Semantic.Unify.Evidence as Evidence (Logical)
import {-# SOURCE #-} Semantic.Unify.Type (Logical)
import Syntax.Position (Position)

newtype Solve s a = Solve (ST s a)

instance Functor (Solve s)

instance Applicative (Solve s)

instance Monad (Solve s)

type SolveType :: ((Environment -> Kind.Type) -> Environment -> Kind.Type) -> Constraint
class SolveType typef where
  solve :: Position -> typef (Logical s) scope -> Solve s (typef Vacuous scope)

instance SolveType TypeF

instance SolveType ConstraintF

instance SolveType ConstraintsF

instance (SolveType typef) => SolveType (ForallOver typef)

type SolveEvidence :: ((Environment -> Kind.Type) -> Environment -> Kind.Type) -> Constraint
class SolveEvidence evidencef where
  solveEvidence :: Position -> evidencef (Evidence.Logical s) scope -> Solve s (evidencef Vacuous scope)

instance SolveEvidence EvidenceF

instance SolveEvidence InstanciationF

runSolve :: Solve s a -> ST s a
liftST :: ST s a -> Solve s a
