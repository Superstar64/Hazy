module Semantic.Resolve.Go.Constraints where

import Semantic.Resolve.Context (Context)
import qualified Semantic.Resolve.Go.Constraint as Constraint
import qualified Semantic.Resolve.Go.Type as Type
import Semantic.Scope (Environment (..), Local)
import Semantic.Stage (Resolve)
import Semantic.Tree.Constraints (Constraints (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.Constraints as Syntax

-- todo: Taking two context here is a horrible hack. Figure out a better way of
-- doing this.
resolve ::
  Context scope ->
  Context (Local ':+ scope) ->
  Syntax.Constraints Position ->
  Constraints Position Resolve scope
resolve context context' = \case
  Syntax.Constraints constraints -> Constraints $ Constraint.resolve context' <$> constraints
  Syntax.Value {levity} -> Value {levity = Type.resolve context levity}
  Syntax.Lazy -> Lazy
  Syntax.Strict -> Strict
