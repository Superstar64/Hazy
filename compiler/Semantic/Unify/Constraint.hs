module Semantic.Unify.Constraint where

import Core.Tree.Constraint (ConstraintF (..))
import qualified Core.Tree.Constraint as Simple
import Semantic.Shift (Shift (..))
import Semantic.Unify.Class (Solve, Zonk (..))
import {-# SOURCE #-} Semantic.Unify.Type (Logical, Type (..))
import {-# SOURCE #-} qualified Semantic.Unify.Type as Type
import Syntax.Position (Position)
import Prelude hiding (Functor, map)

newtype Constraint s scope = Constraintx {runConstraintx :: ConstraintF (Logical s) scope}

instance Zonk Constraint where
  zonk zonker (Constraintx Constraint {classx, head, arguments}) = do
    arguments <- traverse (fmap runTypex . zonk zonker . Typex) arguments
    pure $ Constraintx Constraint {classx, head, arguments}

instance Shift (Constraint s) where
  shift (Constraintx constraint) = Constraintx (shift constraint)

solve :: Position -> ConstraintF (Logical s) scope -> Solve s (Simple.Constraint scope)
solve position Constraint {classx, head, arguments} = do
  arguments <- traverse (Type.solve position) arguments
  pure $
    Simple.Constraint
      { classx,
        head,
        arguments
      }
