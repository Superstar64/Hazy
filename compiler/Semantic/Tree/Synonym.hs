module Semantic.Tree.Synonym where

import qualified Data.Vector.Strict as Strict
import Semantic.FreeVariables (FreeTypeVariables (..))
import qualified Semantic.FreeVariables as FreeVariables
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Type (Type)
import Semantic.Tree.TypePattern (TypePattern)
import Syntax.Position (Position)

data Synonym stage scope = Synonym
  { parameters :: !(Strict.Vector (TypePattern Position stage scope)),
    synonym :: !(Type Position stage (Local ':+ scope))
  }
  deriving (Show)

instance Shift0.Functor (Synonym stage) where
  map = Shift.mapDefault

instance Shift.Functor (Synonym stage) where
  map category Synonym {parameters, synonym} =
    Synonym
      { parameters = Shift.map category <$> parameters,
        synonym = Shift.map (Shift.Over category) synonym
      }

instance FreeTypeVariables Synonym where
  freeTypeVariables target Synonym {synonym} =
    concat
      [ freeTypeVariables (FreeVariables.Over target) synonym
      ]
