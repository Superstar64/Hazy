module Semantic.Unify.Constraints where

import Core.Tree.Constraints (ConstraintsF (..))
import qualified Core.Tree.Constraints as Simple
import Semantic.Shift (Shift (..))
import Semantic.Unify.Class (Solve, Zonk (..))
import Semantic.Unify.Constraint (Constraint (..))
import qualified Semantic.Unify.Constraint as Constraint
import Semantic.Unify.Type (Logical)
import Syntax.Position (Position)

newtype Constraints s scope = Constraintsx {runConstraintsx :: ConstraintsF (Logical s) scope}

instance Zonk Constraints where
  zonk zonker (Constraintsx constraints) = case constraints of
    Constraints constraints ->
      Constraintsx . Constraints
        <$> traverse (fmap runConstraintx . zonk zonker . Constraintx) constraints
    None -> pure (Constraintsx None)

instance Shift (Constraints s) where
  shift (Constraintsx constraints) = Constraintsx $ shift constraints

solve :: Position -> ConstraintsF (Logical s) scope -> Solve s (Simple.Constraints scope)
solve position = \case
  Constraints constraints -> Simple.Constraints <$> traverse (Constraint.solve position) constraints
  None -> pure Simple.None
