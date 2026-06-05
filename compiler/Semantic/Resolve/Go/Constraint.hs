module Semantic.Resolve.Go.Constraint where

import Error (constraintNonLocal, illegalConstraint)
import Semantic.Index.Local (Index (Local))
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Index.Type3 as Type3
import Semantic.Resolve.Context (Context (..), (!$), (!=.*))
import qualified Semantic.Resolve.Go.Type as Type (resolve)
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Stage (Resolve)
import Semantic.Tree.Constraint (Constraint (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.Constraint as Syntax (Constraint (..))
import Prelude hiding (head)

resolve :: Context (Local ':+ scope) -> Syntax.Constraint Position -> Constraint Position Resolve scope
resolve context Syntax.Constraint {startPosition, classVariable, typeVariable, arguments}
  | Local head <- context !$ typeVariable =
      case context !=.* classVariable of
        Type3.Index classx ->
          Constraint
            { startPosition,
              classx = Type2.unlocal classx,
              head,
              arguments = Type.resolve context <$> arguments
            }
        _ -> illegalConstraint startPosition
  | otherwise = constraintNonLocal startPosition
