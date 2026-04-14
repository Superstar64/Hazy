module Stage2.Tree.TypeDeclarationExtra where

import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import Stage2.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Stage2.FreeVariables as FreeTypeVariables
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition (Definition)

data TypeDeclarationExtra scope
  = ADT
  | Class
      { position :: !Position,
        methods :: !(Strict.Vector (Strict.Maybe (Definition (Local ':+ scope))))
      }
  | Synonym
  | GADT
  deriving (Show)

instance Shift TypeDeclarationExtra where
  shift = shiftDefault

instance Shift.Functor TypeDeclarationExtra where
  map category = \case
    ADT -> ADT
    Class {position, methods} ->
      Class
        { position,
          methods = fmap (Shift.map (Shift.Over category)) <$> methods
        }
    Synonym -> Synonym
    GADT -> GADT

instance FreeTermVariables TypeDeclarationExtra where
  freeTermVariables target = \case
    ADT -> []
    Class {methods} -> foldMap (foldMap (freeTermVariables $ FreeTypeVariables.Over target)) methods
    Synonym -> []
    GADT -> []
