module Stage2.Tree.StrictnessAnnotation where

import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Type (Type)
import qualified Stage2.Tree.Type as Type

data StrictnessAnnotation position scope
  = Lazy
  | Strict
  | Polymorphic
      { levity :: !(Type position scope)
      }
  deriving (Show, Eq)

instance Shift (StrictnessAnnotation position) where
  shift = shiftDefault

instance Shift.Functor (StrictnessAnnotation position) where
  map category = \case
    Lazy -> Lazy
    Strict -> Strict
    Polymorphic {levity} ->
      Polymorphic
        { levity = Shift.map category levity
        }

anonymize :: StrictnessAnnotation position scope -> StrictnessAnnotation () scope
anonymize = \case
  Lazy -> Lazy
  Strict -> Strict
  Polymorphic {levity} -> Polymorphic {levity = Type.anonymize levity}
