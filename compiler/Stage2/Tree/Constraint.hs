module Stage2.Tree.Constraint where

import qualified Data.Vector.Strict as Strict (Vector)
import Error (constraintNonLocal, illegalConstraint)
import Stage1.Position (Position)
import qualified Stage1.Tree.Constraint as Stage1 (Constraint (..))
import Stage2.FreeVariables (FreeTypeVariables (..))
import qualified Stage2.FreeVariables as FreeTermVariables
import qualified Stage2.FreeVariables as FreeVariables
import Stage2.Index.Local (Index (Local))
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import Stage2.Resolve.Context (Context (..), (!$), (!=.*))
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Resolve)
import Stage2.Tree.Type (Type)
import qualified Stage2.Tree.Type as Type (anonymize, resolve)
import Prelude hiding (head)

data Constraint position stage scope = Constraint
  { startPosition :: !position,
    classx :: !(Type2.Index scope),
    head :: !Int,
    arguments :: !(Strict.Vector (Type position stage (Local ':+ scope)))
  }
  deriving (Show, Eq)

instance Shift (Constraint position stage) where
  shift = shiftDefault

instance Shift.Functor (Constraint position stage) where
  map category Constraint {startPosition, classx, head, arguments} =
    Constraint
      { startPosition,
        classx = Shift.map category classx,
        head,
        arguments = fmap (Shift.map (Shift.Over category)) arguments
      }

instance FreeTypeVariables (Constraint position) where
  freeTypeVariables target Constraint {classx, arguments} =
    concat
      [ FreeVariables.type2 target classx,
        foldMap (freeTypeVariables $ FreeTermVariables.Over target) arguments
      ]

anonymize :: Constraint position stage scope -> Constraint () stage scope
anonymize Constraint {classx, head, arguments} =
  Constraint
    { startPosition = (),
      classx,
      head,
      arguments = Type.anonymize <$> arguments
    }

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
