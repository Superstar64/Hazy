module Semantic.Check.Temporary.Constraints where

import Control.Monad.ST (ST)
import qualified Core.Tree.Type as Core.Type
import qualified Data.Vector.Strict as Strict
import Semantic.Check.Context (Context)
import Semantic.Check.Temporary.Constraint (Constraint)
import qualified Semantic.Check.Temporary.Constraint as Constraint
import Semantic.Check.Temporary.Type (Type)
import qualified Semantic.Check.Temporary.Type as Type
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Constraints as Semantic
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Constraints s scope
  = Constraints !(Strict.Vector (Constraint s scope))
  | Value {levity :: !(Type s scope)}
  | Lazy
  | Strict

check ::
  Context s (Local ':+ scope) ->
  Semantic.Constraints Position Resolve scope ->
  ST s (Constraints s scope)
check context = \case
  Semantic.Constraints constraints -> do
    constraints <- traverse (Constraint.check context) constraints
    pure $ Constraints constraints
  Semantic.Value {levity} -> do
    levity <- Type.check (Shift.unshift context) Core.Type.levity levity
    pure Value {levity}
  Semantic.Lazy -> pure Lazy
  Semantic.Strict -> pure Strict

solve ::
  Context s (Local ':+ scope) ->
  Constraints s scope ->
  Unify.Solve s (Semantic.Constraints Position Check scope)
solve context = \case
  Constraints constraints -> do
    constraints <- traverse (Constraint.solve context) constraints
    pure $ Semantic.Constraints constraints
  Value {levity} -> do
    levity <- Type.solve (Shift.unshift context) levity
    pure Semantic.Value {levity}
  Lazy -> pure Semantic.Lazy
  Strict -> pure Semantic.Strict
