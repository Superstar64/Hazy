module Semantic.Tree.StrictnessAnnotation where

import Semantic.FreeVariables (FreeTypeVariables (freeTypeVariables))
import Semantic.Shift (Shift (..), shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Tree.Type (Type)
import qualified Semantic.Tree.Type as Type

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

instance FreeTypeVariables (StrictnessAnnotation position) where
  freeTypeVariables target = \case
    Lazy -> []
    Strict -> []
    Polymorphic {levity} -> freeTypeVariables target levity

anonymize :: StrictnessAnnotation position stage scope -> StrictnessAnnotation () stage scope
anonymize = \case
  Lazy -> Lazy
  Strict -> Strict
  Polymorphic {levity} -> Polymorphic {levity = Type.anonymize levity}
