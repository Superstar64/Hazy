module Semantic.Check.Go.Scheme (Scheme (..), augment) where

import Control.Monad.ST (ST)
import qualified Core.Tree.Constraints as Simple.Constraints
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context (..))
import Semantic.Check.Mask (Mask)
import Semantic.Check.Simple.SchemeOver (augmentNamed)
import Semantic.Scope (Environment (..), Local)
import Semantic.Stage (Check)
import Semantic.Tree.Constraints (Constraints)
import Semantic.Tree.Scheme (Scheme (..))
import Semantic.Tree.TypePattern (TypePattern (TypePattern))
import qualified Semantic.Tree.TypePattern as TypePattern
import Syntax.Position (Position)
import Prelude hiding (head)

augment ::
  Position ->
  Strict.Vector (TypePattern Position Check scope) ->
  Constraints Position Check scope ->
  Mask ->
  Context s scope ->
  ST s (Context s (Local ':+ scope))
augment position parameters constraints =
  augmentNamed
    name
    position
    (TypePattern.typex' <$> parameters)
    (Simple.Constraints.simplify constraints)
  where
    name i = case parameters Strict.Vector.! i of
      TypePattern {name} -> name
