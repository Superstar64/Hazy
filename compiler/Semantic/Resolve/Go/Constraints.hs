module Semantic.Resolve.Go.Constraints where

import Semantic.Resolve.Context (Context)
import qualified Semantic.Resolve.Go.Constraint as Constraint
import Semantic.Scope (Environment (..), Local)
import Semantic.Stage (Resolve)
import Semantic.Tree.Constraints (Constraints (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.Constraints as Syntax

resolve :: Context (Local ':+ scope) -> Syntax.Constraints Position -> Constraints Position Resolve scope
resolve context = \case
  Syntax.Constraints constraints -> Constraints $ Constraint.resolve context <$> constraints
  Syntax.None -> None
