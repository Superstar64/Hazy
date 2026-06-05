module Semantic.Tree.Constraint where

import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.FreeVariables (FreeTypeVariables (..))
import qualified Semantic.FreeVariables as FreeTermVariables
import qualified Semantic.FreeVariables as FreeVariables
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Tree.Type (Type)
import qualified Semantic.Tree.Type as Type (anonymize)
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
