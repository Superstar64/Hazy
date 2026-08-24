module Semantic.Tree.InstanceDefinition2 where

import Data.Functor.Classes (Show1, showsPrec1)
import Data.Functor.Identity (Identity (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Connect (Connect (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Constraints (Constraints)
import Semantic.Tree.InstanceDefinition (InstanceDefinition)
import Semantic.Tree.TypePattern (TypePattern)
import Syntax.Position (Position)

data InstanceDefinition2 solve layout stage scope
  = (:::)
      !(Annotation stage scope)
      !(solve (InstanceDefinition layout stage scope))

infix 5 :::

instance (Show1 solve) => Show (InstanceDefinition2 solve layout stage scope) where
  showsPrec d (annotation ::: definition) =
    showParen (d > 6) $
      showsPrec 6 annotation . showString " ::: " . showsPrec1 6 definition

instance (Functor solve) => Shift0.Functor (InstanceDefinition2 solve layout stage) where
  map = Shift.mapDefault

instance (Functor solve) => Shift.Functor (InstanceDefinition2 solve layout stage) where
  map category (annotation ::: definition) = Shift.map category annotation ::: fmap (Shift.map category) definition

instance (solve ~ Identity) => Connect (InstanceDefinition2 solve) where
  connect (annotation ::: Identity definition) = annotation ::: Identity (connect definition)
  seperate (annotation ::: Identity definition) = annotation ::: Identity (seperate definition)

data Annotation stage scope = Annotation
  { parameters :: !(Strict.Vector (TypePattern Position stage scope)),
    prerequisites :: !(Constraints Position stage scope)
  }
  deriving (Show)

instance Shift0.Functor (Annotation stage) where
  map = Shift.mapDefault

instance Shift.Functor (Annotation stage) where
  map category Annotation {parameters, prerequisites} =
    Annotation
      { parameters = Shift.map category <$> parameters,
        prerequisites = Shift.map category prerequisites
      }
