module Stage2.Tree.Annotation where

import Stage1.Position (Position)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Scheme (Scheme)

data Mark
  = Annotated
  | Inferred

type Annotated = 'Annotated

type Inferred = 'Inferred

data Annotation mark scope where
  Annotation :: !(Scheme Position scope) -> Annotation Annotated scope
  NoAnnotation :: Annotation Inferred scope

instance Show (Annotation mark scope) where
  showsPrec d (Annotation scheme) =
    showParen (d > 10) $
      showString "Annotation " . showsPrec 11 scheme
  showsPrec _ NoAnnotation = showString "NoAnnotation"

instance Shift (Annotation mark) where
  shift = shiftDefault

instance Shift.Functor (Annotation mark) where
  map category = \case
    Annotation scheme -> Annotation (Shift.map category scheme)
    NoAnnotation -> NoAnnotation
