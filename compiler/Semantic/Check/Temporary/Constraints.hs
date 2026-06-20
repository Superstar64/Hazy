module Semantic.Check.Temporary.Constraints where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import Semantic.Check.Context (Context)
import Semantic.Check.Temporary.Constraint (Constraint)
import qualified Semantic.Check.Temporary.Constraint as Constraint
import Semantic.Scope (Environment (..), Local)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Constraints as Semantic
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Constraints s scope
  = Constraints !(Strict.Vector (Constraint s scope))
  | None

check ::
  Context s (Local ':+ scope) ->
  Semantic.Constraints Position Resolve scope ->
  ST s (Constraints s scope)
check context = \case
  Semantic.Constraints constraints -> do
    constraints <- traverse (Constraint.check context) constraints
    pure $ Constraints constraints
  Semantic.None -> pure None

solve ::
  Context s (Local ':+ scope) ->
  Constraints s scope ->
  Unify.Solve s (Semantic.Constraints Position Check scope)
solve context = \case
  Constraints constraints -> do
    constraints <- traverse (Constraint.solve context) constraints
    pure $ Semantic.Constraints constraints
  None -> pure Semantic.None
