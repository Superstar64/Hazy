module Stage2.Tree.TypeDeclarationExtra where

import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Stage2.FreeVariables as FreeTypeVariables
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition (Definition)

data TypeDeclarationExtra layout stage scope
  = ADT {position :: !Position}
  | Class
      { position :: !Position,
        methods :: !(Strict.Vector (Strict.Maybe (Definition layout stage (Local ':+ scope))))
      }
  | Synonym {position :: !Position}
  | GADT {position :: !Position}
  deriving (Show)

instance Shift (TypeDeclarationExtra layout stage) where
  shift = shiftDefault

instance Shift.Functor (TypeDeclarationExtra layout stage) where
  map category = \case
    ADT {position} -> ADT {position}
    Class {position, methods} ->
      Class
        { position,
          methods = fmap (Shift.map (Shift.Over category)) <$> methods
        }
    Synonym {position} -> Synonym {position}
    GADT {position} -> GADT {position}

instance FreeTermVariables (TypeDeclarationExtra layout) where
  freeTermVariables target = \case
    ADT {} -> []
    Class {methods} -> foldMap (foldMap (freeTermVariables $ FreeTypeVariables.Over target)) methods
    Synonym {} -> []
    GADT {} -> []

instance Connect TypeDeclarationExtra where
  connect = \case
    ADT {position} -> ADT {position}
    Class {position, methods} ->
      Class
        { position,
          methods = fmap connect <$> methods
        }
    Synonym {position} -> Synonym {position}
    GADT {position} -> GADT {position}
