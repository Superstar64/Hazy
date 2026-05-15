module Stage2.Tree.StrictnessAnnotation where

import Stage2.FreeVariables (FreeTypeVariables (freeTypeVariables))
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Type (Type)
import qualified Stage2.Tree.Type as Type

data StrictnessAnnotation position stage scope
  = Lazy
  | Strict
  | Polymorphic
      { levity :: !(Type position stage scope)
      }
  deriving (Show, Eq)

instance Shift (StrictnessAnnotation position stage) where
  shift = shiftDefault

instance Shift.Functor (StrictnessAnnotation position stage) where
  map category = \case
    Lazy -> Lazy
    Strict -> Strict
    Polymorphic {levity} ->
      Polymorphic
        { levity = Shift.map category levity
        }

instance FreeTypeVariables (StrictnessAnnotation position stage) where
  freeTypeVariables target = \case
    Lazy -> []
    Strict -> []
    Polymorphic {levity} -> freeTypeVariables target levity

anonymize :: StrictnessAnnotation position stage scope -> StrictnessAnnotation () stage scope
anonymize = \case
  Lazy -> Lazy
  Strict -> Strict
  Polymorphic {levity} -> Polymorphic {levity = Type.anonymize levity}
