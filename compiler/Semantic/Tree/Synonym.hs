module Semantic.Tree.Synonym where

import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.FreeVariables (FreeTypeVariables (..))
import qualified Semantic.FreeVariables as FreeVariables
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Type (Type)
import Semantic.Tree.TypePattern (TypePattern)
import Syntax.Position (Position)

data Synonym stage scope
  = (:::)
      !(Strict.Maybe (Type Position stage scope))
      !(SynonymBody stage scope)
  deriving (Show)

infix 5 :::

instance Shift0.Functor (Synonym stage) where
  map = Shift.mapDefault

instance Shift.Functor (Synonym stage) where
  map category (annotation ::: body) = fmap (Shift.map category) annotation ::: Shift.map category body

instance FreeTypeVariables Synonym where
  freeTypeVariables target (annotation ::: body) =
    foldMap (freeTypeVariables target) annotation ++ freeTypeVariables target body

data SynonymBody stage scope = SynonymBody
  { parameters :: !(Strict.Vector (TypePattern Position stage scope)),
    synonym :: !(Type Position stage (Local ':+ scope))
  }
  deriving (Show)

instance Shift0.Functor (SynonymBody stage) where
  map = Shift.mapDefault

instance Shift.Functor (SynonymBody stage) where
  map category SynonymBody {parameters, synonym} =
    SynonymBody
      { parameters = Shift.map category <$> parameters,
        synonym = Shift.map (Shift.Over category) synonym
      }

instance FreeTypeVariables SynonymBody where
  freeTypeVariables target SynonymBody {synonym} =
    concat
      [ freeTypeVariables (FreeVariables.Over target) synonym
      ]
