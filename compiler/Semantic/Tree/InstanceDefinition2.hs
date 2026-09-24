module Semantic.Tree.InstanceDefinition2 where

import Data.Functor.Classes (Show1, showsPrec1)
import Data.Functor.Compose (getCompose)
import Data.Functor.Identity (Identity (..))
import Data.NaturalTransformation (NaturalTransformation (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Connect (Connect (..))
import Semantic.Functor2 (Traversable2 (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Constraints (Constraints)
import Semantic.Tree.InstanceDefinition (InstanceDefinition)
import Semantic.Tree.TypePattern (TypePattern)
import Syntax.Position (Position)

data InstanceDefinition2 solve loeb layout stage scope where
  (:::) ::
    loeb (Annotation stage scope) ->
    loeb (solve (InstanceDefinition origin layout stage scope)) ->
    InstanceDefinition2 solve loeb layout stage scope

infix 5 :::

instance (Show1 solve, Show1 loeb) => Show (InstanceDefinition2 solve loeb layout stage scope) where
  showsPrec d (annotation ::: definition) =
    showParen (d > 6) $
      showsPrec 6 annotation . showString " ::: " . showsPrec1 6 definition

instance (Functor solve, Functor loeb) => Shift0.Functor (InstanceDefinition2 solve loeb layout stage) where
  map = Shift.mapDefault

instance (Functor solve, Functor loeb) => Shift.Functor (InstanceDefinition2 solve loeb layout stage) where
  map category (annotation ::: definition) =
    fmap (Shift.map category) annotation ::: fmap (fmap (Shift.map category)) definition

instance Traversable2 (InstanceDefinition2 solve) where
  traverse2 (Morph f) (annotation ::: definition) =
    (:::) <$> getCompose (f annotation) <*> getCompose (f definition)

instance (solve ~ Identity, loeb ~ Identity) => Connect (InstanceDefinition2 solve loeb) where
  connect (Identity annotation ::: Identity (Identity definition)) =
    Identity annotation ::: Identity (Identity (connect definition))
  seperate (Identity annotation ::: Identity (Identity definition)) =
    Identity annotation ::: Identity (Identity (seperate definition))

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
