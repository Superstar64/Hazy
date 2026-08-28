module Semantic.Tree.TypeGroup where

import qualified Core.Tree.Type as Simple
import Data.Kind (Type)
import qualified Data.Vector.Strict as Strict
import Semantic.FreeVariables (FreeTypeVariables (..))
import qualified Semantic.FreeVariables as FreeVariables
import qualified Semantic.Index.Link.Type as Type
import qualified Semantic.Label.Binding.Type as Label
import Semantic.Layout (Layout)
import Semantic.Locality (Locality)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Stage)
import Semantic.Tree.Combinators.Inferred (Inferred)
import qualified Semantic.Tree.Combinators.Inferred as Combinators
import Semantic.Tree.TypeDefinition (TypeDefinition)
import Syntax.Position (Position)
import Syntax.Variable (QualifiedConstructor (..), QualifiedConstructorIdentifier (..))

type TypeGroup :: Locality -> Layout -> Stage -> Environment -> Type
data TypeGroup locality layout stage scope
  = (::::)
      !(Inferred Types stage scope)
      !(Set locality stage scope)
  deriving (Show)

infix 5 ::::

instance Shift0.Functor (TypeGroup locality layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (TypeGroup locality layout stage) where
  map category (types :::: set) = Shift.map category types :::: Shift.map category set

instance FreeTypeVariables (TypeGroup locality layout) where
  freeTypeVariables target (_ :::: set) = freeTypeVariables target set

newtype Types scope = Types (Strict.Vector (Simple.Type scope))
  deriving (Show)

instance Scope.Show Types where
  showsPrec = showsPrec

instance Shift0.Functor Types where
  map = Shift.mapDefault

instance Shift.Functor Types where
  map category (Types types) = Types (Shift.map category <$> types)

newtype Set locality stage scope
  = Set (Strict.Vector (Element locality stage scope))
  deriving (Show)

instance Shift0.Functor (Set locality stage) where
  map = Shift.mapDefault

instance Shift.Functor (Set locality stage) where
  map category (Set set) = Set (Shift.map category <$> set)

instance FreeTypeVariables (Set locality) where
  freeTypeVariables target (Set set) = foldMap (freeTypeVariables target) set

data Element locality stage scope = Element
  { element :: !(TypeDefinition stage (Scope.GroupType ':+ scope)),
    typex :: !(Combinators.Inferred Simple.Type stage scope),
    position :: !Position,
    name :: !QualifiedConstructorIdentifier,
    constructorNames :: !(Strict.Vector QualifiedConstructor),
    link :: !(Type.Link locality)
  }
  deriving (Show)

instance Shift0.Functor (Element locality stage) where
  map = Shift.mapDefault

instance Shift.Functor (Element locality stage) where
  map category Element {element, typex, position, name, constructorNames, link} =
    Element
      { element = Shift.map (Shift.Over category) element,
        typex = Shift.map category typex,
        position,
        name,
        constructorNames,
        link
      }

instance FreeTypeVariables (Element locality) where
  freeTypeVariables target Element {element} =
    freeTypeVariables (FreeVariables.Over target) element

label :: Element locality stage scope1 -> Label.TypeBinding scope2
label Element {name, constructorNames} =
  Label.TypeBinding
    { name,
      constructorNames
    }
