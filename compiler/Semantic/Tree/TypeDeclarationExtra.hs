module Semantic.Tree.TypeDeclarationExtra where

import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Semantic.FreeVariables as FreeVariables
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.MethodAbstract (MethodAbstract)
import Syntax.Position (Position)

data TypeDeclarationExtra layout stage scope
  = ADT {position :: !Position}
  | Class
      { position :: !Position,
        methods :: !(Strict.Vector (MethodAbstract layout stage (Local ':+ scope)))
      }
  | Synonym {position :: !Position}
  | GADT {position :: !Position}
  deriving (Show)

instance Shift0.Functor (TypeDeclarationExtra layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (TypeDeclarationExtra layout stage) where
  map category = \case
    ADT {position} -> ADT {position}
    Class {position, methods} ->
      Class
        { position,
          methods = Shift.map (Shift.Over category) <$> methods
        }
    Synonym {position} -> Synonym {position}
    GADT {position} -> GADT {position}

instance FreeTermVariables (TypeDeclarationExtra layout) where
  freeTermVariables target = \case
    ADT {} -> []
    Class {methods} -> foldMap (freeTermVariables $ FreeVariables.Over target) methods
    Synonym {} -> []
    GADT {} -> []

instance Connect TypeDeclarationExtra where
  connect = \case
    ADT {position} -> ADT {position}
    Class {position, methods} ->
      Class
        { position,
          methods = connect <$> methods
        }
    Synonym {position} -> Synonym {position}
    GADT {position} -> GADT {position}
  seperate = \case
    ADT {position} -> ADT {position}
    Class {position, methods} ->
      Class
        { position,
          methods = seperate <$> methods
        }
    Synonym {position} -> Synonym {position}
    GADT {position} -> GADT {position}
