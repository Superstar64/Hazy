module Semantic.Unify.Constraint where

import Core.Tree.Constraint (ConstraintF (..))
import qualified Core.Tree.Constraint as Simple
import Semantic.Unify.Solve (Solve)
import {-# SOURCE #-} Semantic.Unify.Type (Logical)
import {-# SOURCE #-} qualified Semantic.Unify.Type as Type
import Syntax.Position (Position)
import Prelude hiding (Functor, map)

type Constraint s = ConstraintF (Logical s)

solve :: Position -> ConstraintF (Logical s) scope -> Solve s (Simple.Constraint scope)
solve position Constraint {classx, head, arguments} = do
  arguments <- traverse (Type.solve position) arguments
  pure $
    Simple.Constraint
      { classx,
        head,
        arguments
      }
