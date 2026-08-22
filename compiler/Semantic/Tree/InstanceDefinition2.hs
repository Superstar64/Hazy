module Semantic.Tree.InstanceDefinition2 where

import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Connect (Connect (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Constraints (Constraints)
import Semantic.Tree.InstanceDefinition (InstanceDefinition)
import Semantic.Tree.TypePattern (TypePattern)
import Syntax.Position (Position)

data InstanceDefinition2 layout stage scope
  = (:::)
      (Annotation stage scope)
      (InstanceDefinition layout stage scope)
  deriving (Show)

infix 5 :::

instance Shift0.Functor (InstanceDefinition2 layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (InstanceDefinition2 layout stage) where
  map category (annotation ::: definition) = Shift.map category annotation ::: Shift.map category definition

instance Connect InstanceDefinition2 where
  connect (annotation ::: definition) = annotation ::: connect definition
  seperate (annotation ::: definition) = annotation ::: seperate definition

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
