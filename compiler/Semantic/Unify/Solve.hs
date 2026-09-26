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
import qualified Core.Type.Functor as Core
import Data.Kind (Constraint)
import qualified Data.Kind as Kind
import Data.STRef (readSTRef)
import Data.Void (Void)
import Error (Position, unsupportedFeatureConstraintedTypeDefaulting)
import Semantic.Scope (Environment)
import qualified Semantic.Shift0 as Shift0
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
solve = solveWith Shift0.Id

instance SolveType TypeF where
  solveWith category position = \case
    Type.Logical (Type.Box reference) ->
      Solve (readSTRef reference) >>= \case
        Type.Solved typex ->
          Shift0.map category <$> solve position typex
        Type.Unsolved {kind, constraints} ->
          Solve $ defaultFrom position constraints (Core.mapLogical category kind)
    Type.Logical (Type.Shift logical) ->
      solveWith (category Shift0.:. Shift0.Shift) position (Type.Logical logical)
    Type.Variable name -> pure $ Type.Variable name
    Type.Constructor index -> pure $ Type.Constructor index
    Type.Call function argument -> do
      function <- solveWith category position function
      argument <- solveWith category position argument
      pure $ Type.Call function argument
    Type.Function argument result -> do
      argument <- solveWith category position argument
      result <- solveWith category position result
      pure $ Type.Function argument result
    Type.Type universe -> do
      universe <- solveWith category position universe
      pure $ Type.Type universe
    Type.Constraint -> pure Type.Constraint
    Type.Small -> pure Type.Small
    Type.Large -> pure Type.Large
    Type.Universe -> pure Type.Universe
    Type.Levity -> pure Type.Levity

instance SolveType ConstraintF where
  solveWith category position Constraint {classx, head, arguments} = do
    arguments <- traverse (solveWith (Shift0.Shift Shift0.:. category) position) arguments
    pure Constraint {classx, head, arguments}

instance SolveType ConstraintsF where
  solveWith category position = \case
    Constraints constraints -> do
      constraints <- traverse (solveWith category position) constraints
      pure $ Constraints constraints
    None -> pure None

instance (SolveType typef) => SolveType (ForallOver typef) where
  solveWith category position ForallOver {parameters, constraints, result} = do
    parameters <- traverse (solveWith category position) parameters
    constraints <- solveWith category position constraints
    result <- solveWith (Shift0.Shift Shift0.:. category) position result
    pure ForallOver {parameters, constraints, result}

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
solveEvidence = solveEvidenceWith Shift0.Id

instance SolveEvidence EvidenceF where
  solveEvidenceWith category position = \case
    Evidence.Variable variable instanciation -> do
      instanciation <- solveEvidenceWith category position instanciation
      pure Evidence.Variable {variable, instanciation}
    Evidence.Super base index -> do
      base <- solveEvidenceWith category position base
      pure $ Evidence.Super {base, index}
    Evidence.Logical (Evidence.Shift evidence) ->
      solveEvidenceWith (category Shift0.:. Shift0.Shift) position (Evidence.Logical evidence)
    Evidence.Logical (Evidence.Box reference) ->
      Solve (readSTRef reference) >>= \case
        Evidence.Solved evidence -> Shift0.map category <$> solveEvidence position evidence
        Evidence.Unsolved -> unsupportedFeatureConstraintedTypeDefaulting position

instance SolveEvidence InstanciationF where
  solveEvidenceWith category position = \case
    Instanciation instanciation -> do
      instanciation <- traverse (solveEvidenceWith category position) instanciation
      pure $ Instanciation instanciation
    Mono -> pure Mono

-- todo, figure out how to make sure nodes that are being solved early have a
-- rigid context
runSolve :: Solve s a -> ST s a
runSolve (Solve a) = a
