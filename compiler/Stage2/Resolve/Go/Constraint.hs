module Stage2.Resolve.Go.Constraint where

import Error (constraintNonLocal, illegalConstraint)
import Stage1.Position (Position)
import qualified Stage1.Tree.Constraint as Stage1 (Constraint (..))
import Stage2.Index.Local (Index (Local))
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import Stage2.Resolve.Context (Context (..), (!$), (!=.*))
import qualified Stage2.Resolve.Go.Type as Type (resolve)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Stage (Resolve)
import Stage2.Tree.Constraint (Constraint (..))
import Prelude hiding (head)

resolve :: Context (Local ':+ scope) -> Stage1.Constraint Position -> Constraint Position Resolve scope
resolve context Stage1.Constraint {startPosition, classVariable, typeVariable, arguments}
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
