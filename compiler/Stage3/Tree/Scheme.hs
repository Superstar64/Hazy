module Stage3.Tree.Scheme where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import Stage2.Scope (Environment (..), Local)
import Stage3.Check.Context (Context (..))
import Stage3.Simple.SchemeOver (augmentNamed)
import Stage3.Tree.Constraint (Constraint (..))
import Stage3.Tree.Type (Type)
import Stage3.Tree.TypePattern (TypePattern (TypePattern))
import qualified Stage3.Tree.TypePattern as TypePattern
import qualified Stage4.Tree.Constraint as Simple.Constraint
import Prelude hiding (head)

data Scheme scope = Scheme
  { parameters :: !(Strict.Vector (TypePattern scope)),
    constraints :: !(Strict.Vector (Constraint scope)),
    result :: !(Type (Local ':+ scope))
  }
  deriving (Show)

augment ::
  Position ->
  Strict.Vector (TypePattern scope) ->
  Strict.Vector (Constraint scope) ->
  Context s scope ->
  ST s (Context s (Local ':+ scope))
augment position parameters constraints =
  augmentNamed
    name
    position
    (TypePattern.typex <$> parameters)
    (Simple.Constraint.simplify <$> constraints)
  where
    name i = case parameters Strict.Vector.! i of
      TypePattern {name} -> name
