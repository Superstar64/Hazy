module Semantic.Unify.Constraints where

import Core.Tree.Constraints (ConstraintsF (..))
import qualified Core.Tree.Constraints as Simple
import qualified Semantic.Unify.Constraint as Constraint
import Semantic.Unify.Solve (Solve)
import Semantic.Unify.Type (Logical)
import Syntax.Position (Position)

type Constraints s = ConstraintsF (Logical s)

solve :: Position -> ConstraintsF (Logical s) scope -> Solve s (Simple.Constraints scope)
solve position = \case
  Constraints constraints -> Simple.Constraints <$> traverse (Constraint.solve position) constraints
  None -> pure Simple.None
