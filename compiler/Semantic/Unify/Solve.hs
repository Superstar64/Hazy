module Semantic.Unify.Solve where

import Control.Monad (ap, liftM)
import Control.Monad.ST (ST)
import Core.Tree.Constraint (ConstraintF (..))
import Core.Tree.Constraints (ConstraintsF (..))
import Core.Tree.Evidence (EvidenceF)
import qualified Core.Tree.Evidence as Evidence (EvidenceF (..))
import Core.Tree.Forall (ForallOver (..))
import Core.Tree.Instanciation (InstanciationF (..))
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Type (TypeF (..))
import Data.Kind (Constraint)
import qualified Data.Kind as Kind
import Data.STRef (readSTRef)
import Error (Position, unsupportedFeatureConstraintedTypeDefaulting)
import Semantic.Scope (Environment, Vacuous)
import Semantic.Shift0 (shift)
import qualified Semantic.Unify.Evidence as Evidence (Box (..), Logical (..))
import Semantic.Unify.Type (Logical, defaultFrom)
import qualified Semantic.Unify.Type as Type (Box (..), Logical (..))

newtype Solve s a = Solve (ST s a)

instance Functor (Solve s) where
  fmap = liftM

instance Applicative (Solve s) where
  pure a = Solve (pure a)
  (<*>) = ap

instance Monad (Solve s) where
  Solve m >>= f = Solve (m >>= (\(Solve a) -> a) . f)

type SolveType :: ((Environment -> Kind.Type) -> Environment -> Kind.Type) -> Constraint
class SolveType typef where
  solve :: Position -> typef (Logical s) scope -> Solve s (typef Vacuous scope)

instance SolveType TypeF where
  solve position = solve
    where
      solve :: TypeF (Logical s) scope -> Solve s (TypeF Vacuous scope)
      solve = \case
        Type.Logical (Type.Box reference) ->
          Solve (readSTRef reference) >>= \case
            Type.Solved typex -> solve typex
            Type.Unsolved {kind, constraints} -> Solve $ defaultFrom position constraints kind
        Type.Logical (Type.Shift logical) -> shift <$> solve (Type.Logical logical)
        Type.Variable name -> pure $ Type.Variable name
        Type.Constructor index -> pure $ Type.Constructor index
        Type.Call function argument -> do
          function <- solve function
          argument <- solve argument
          pure $ Type.Call function argument
        Type.Function argument result -> do
          argument <- solve argument
          result <- solve result
          pure $ Type.Function argument result
        Type.Type universe -> do
          universe <- solve universe
          pure $ Type.Type universe
        Type.Constraint -> pure Type.Constraint
        Type.Small -> pure Type.Small
        Type.Large -> pure Type.Large
        Type.Universe -> pure Type.Universe
        Type.Levity -> pure Type.Levity

instance SolveType ConstraintF where
  solve position Constraint {classx, head, arguments} = do
    arguments <- traverse (solve position) arguments
    pure Constraint {classx, head, arguments}

instance SolveType ConstraintsF where
  solve position = \case
    Constraints constraints -> do
      constraints <- traverse (solve position) constraints
      pure $ Constraints constraints
    None -> pure None

instance (SolveType typef) => SolveType (ForallOver typef) where
  solve position ForallOver {parameters, constraints, result} = do
    parameters <- traverse (solve position) parameters
    constraints <- solve position constraints
    result <- solve position result
    pure ForallOver {parameters, constraints, result}

type SolveEvidence :: ((Environment -> Kind.Type) -> Environment -> Kind.Type) -> Constraint
class SolveEvidence evidencef where
  solveEvidence :: Position -> evidencef (Evidence.Logical s) scope -> Solve s (evidencef Vacuous scope)

instance SolveEvidence EvidenceF where
  solveEvidence position = \case
    Evidence.Variable variable instanciation -> do
      instanciation <- solveEvidence position instanciation
      pure Evidence.Variable {variable, instanciation}
    Evidence.Super base index -> do
      base <- solveEvidence position base
      pure $ Evidence.Super {base, index}
    Evidence.Logical (Evidence.Shift evidence) -> shift <$> solveEvidence position (Evidence.Logical evidence)
    Evidence.Logical (Evidence.Box reference) ->
      Solve (readSTRef reference) >>= \case
        Evidence.Solved evidence -> solveEvidence position evidence
        Evidence.Unsolved -> unsupportedFeatureConstraintedTypeDefaulting position

instance SolveEvidence InstanciationF where
  solveEvidence position = \case
    Instanciation instanciation -> do
      instanciation <- traverse (solveEvidence position) instanciation
      pure $ Instanciation instanciation
    Mono -> pure Mono

-- todo, figure out how to make sure nodes that are being solved early have a
-- rigid context
runSolve :: Solve s a -> ST s a
runSolve (Solve a) = a

-- todo, figure out how to make sure no unification occurs during solving
liftST :: ST s a -> Solve s a
liftST = Solve
