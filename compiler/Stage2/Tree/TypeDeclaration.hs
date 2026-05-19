{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.TypeDeclaration
  ( TypeDeclaration (..),
    labelBinding,
    locality,
    group,
  )
where

import qualified Data.Kind
import qualified Data.Vector.Strict as Strict
import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Position (Position)
import Stage1.Variable
  ( Constructor,
    ConstructorIdentifier,
    QualifiedConstructor ((:=)),
    QualifiedConstructorIdentifier ((:=.)),
    Qualifiers,
  )
import Stage2.FreeVariables (FreeTypeVariables (..))
import qualified Stage2.Index.Link.Type as Type
import qualified Stage2.Index.Type0 as Type0
import qualified Stage2.Label.Binding.Type as Label
import Stage2.Layout (Group, Layout, Normal)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Resolve)
import Stage2.Tree.TypeDefinition (TypeDefinition)
import Stage2.Tree.TypeDefinition2 (TypeDefinition2)
import qualified Stage2.Tree.TypeDefinition2 as TypeDefinition2

type TypeDeclaration :: Locality -> Layout -> Environment -> Data.Kind.Type
data TypeDeclaration locality layout scope
  = TypeDeclaration
  { position :: !Position,
    name :: !ConstructorIdentifier,
    constructorNames :: !(Strict.Vector Constructor),
    definition :: !(TypeDefinition2 locality layout Resolve scope)
  }
  deriving (Show)

instance Shift (TypeDeclaration locality layout) where
  shift = shiftDefault

instance Shift.Functor (TypeDeclaration locality layout) where
  map category = \case
    TypeDeclaration {position, name, constructorNames, definition} ->
      TypeDeclaration
        { position,
          name,
          constructorNames,
          definition = Shift.map category definition
        }

instance FreeTypeVariables (TypeDeclaration locality layout) where
  freeTypeVariables target = \case
    TypeDeclaration {definition} -> freeTypeVariables target definition

labelBinding :: Qualifiers -> TypeDeclaration locality layout scope -> Label.TypeBinding scope'
labelBinding path declaration =
  Label.TypeBinding
    { name = path :=. name declaration,
      constructorNames = (path :=) <$> constructorNames declaration
    }

locality :: TypeDeclaration locality Normal scope -> TypeDeclaration locality' Normal scope
locality = \case
  TypeDeclaration {position, name, constructorNames, definition} ->
    TypeDeclaration
      { position,
        name,
        constructorNames,
        definition = TypeDefinition2.locality definition
      }

group ::
  (Type0.Index scope -> Type.Link locality) ->
  (Type.Link locality -> TypeDefinition Resolve scope) ->
  StronglyConnected.Component (Type.Link locality) ->
  TypeDeclaration locality Normal scope ->
  TypeDeclaration locality Group scope
group link index group = \case
  TypeDeclaration {position, name, constructorNames, definition} ->
    TypeDeclaration
      { position,
        name,
        constructorNames,
        definition = TypeDefinition2.group link index group definition
      }
