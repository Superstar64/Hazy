module Stage2.Check.Go.Scheme (Scheme (..), augment) where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import Stage2.Scope (Environment (..), Local)
import Stage2.Stage (Check)
import Stage2.Tree.Constraint (Constraint (..))
import Stage2.Tree.Scheme (Scheme (..))
import Stage2.Tree.TypePattern (TypePattern (TypePattern))
import qualified Stage2.Tree.TypePattern as TypePattern
import Stage2.Check.Context (Context (..))
import Stage2.Check.Mask (Mask)
import Stage2.Check.Simple.SchemeOver (augmentNamed)
import qualified Stage4.Tree.Constraint as Simple.Constraint
import Prelude hiding (head)

augment ::
  Position ->
  Strict.Vector (TypePattern Position Check scope) ->
  Strict.Vector (Constraint Position Check scope) ->
  Mask ->
  Context s scope ->
  ST s (Context s (Local ':+ scope))
augment position parameters constraints =
  augmentNamed
    name
    position
    (TypePattern.typex' <$> parameters)
    (Simple.Constraint.simplify <$> constraints)
  where
    name i = case parameters Strict.Vector.! i of
      TypePattern {name} -> name
