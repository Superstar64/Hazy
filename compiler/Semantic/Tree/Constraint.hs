module Semantic.Tree.Constraint where

import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.FreeVariables (FreeTypeVariables (..))
import qualified Semantic.FreeVariables as FreeTermVariables
import qualified Semantic.FreeVariables as FreeVariables
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment ((:+)), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Unsupported)
import Semantic.Tree.Type (Type)
import qualified Semantic.Tree.Type as Type (anonymize)
import Prelude hiding (head)

data Constraint position stage scope
  = Constraint
      { startPosition :: !position,
        classx :: !(Type2.Index scope),
        head :: !Int,
        arguments :: !(Strict.Vector (Type position stage (Local ':+ scope)))
      }
  | Equality
      { startPosition :: !position,
        left :: !(Type position stage (Local ':+ scope)),
        right :: !(Type position stage (Local ':+ scope)),
        unsupported :: !(Unsupported stage)
      }
  deriving (Show, Eq)

instance Shift0.Functor (Constraint position stage) where
  map = Shift.mapDefault

instance Shift.Functor (Constraint position stage) where
  map category = \case
    Constraint {startPosition, classx, head, arguments} ->
      Constraint
        { startPosition,
          classx = Shift.map category classx,
          head,
          arguments = fmap (Shift.map (Shift.Over category)) arguments
        }
    Equality {startPosition, left, right, unsupported} ->
      Equality
        { startPosition,
          left = Shift.map (Shift.Over category) left,
          right = Shift.map (Shift.Over category) right,
          unsupported
        }

instance FreeTypeVariables (Constraint position) where
  freeTypeVariables target = \case
    Constraint {classx, arguments} ->
      concat
        [ FreeVariables.type2 target classx,
          foldMap (freeTypeVariables $ FreeTermVariables.Over target) arguments
        ]
    Equality {left, right} ->
      concat
        [ freeTypeVariables (FreeTermVariables.Over target) left,
          freeTypeVariables (FreeTermVariables.Over target) right
        ]

anonymize :: Constraint position stage scope -> Constraint () stage scope
anonymize = \case
  Constraint {classx, head, arguments} ->
    Constraint
      { startPosition = (),
        classx,
        head,
        arguments = Type.anonymize <$> arguments
      }
  Equality {left, right, unsupported} ->
    Equality
      { startPosition = (),
        left = Type.anonymize left,
        right = Type.anonymize right,
        unsupported
      }
