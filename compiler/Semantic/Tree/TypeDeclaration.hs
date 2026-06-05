{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.TypeDeclaration where

import qualified Core.Tree.Type as Simple (Type)
import qualified Data.Kind
import qualified Data.Vector.Strict as Strict
import qualified Graph.StronglyConnected as StronglyConnected
import Semantic.FreeVariables (FreeTypeVariables (..), Target (..))
import qualified Semantic.Index.Link.Type as Type
import qualified Semantic.Index.Type0 as Type0
import qualified Semantic.Label.Binding.Type as Label
import Semantic.Layout (Group, Layout, Normal)
import Semantic.Locality (Locality)
import Semantic.Scope (Environment (..))
import Semantic.Shift (Shift, shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve, Stage)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.TypeDefinition (Constructive, TypeDefinition)
import Semantic.Tree.TypeDefinition2 (TypeDefinition2)
import qualified Semantic.Tree.TypeDefinition2 as TypeDefinition2
import Syntax.Position (Position)
import Syntax.Variable
  ( Constructor,
    ConstructorIdentifier,
    QualifiedConstructor ((:=)),
    QualifiedConstructorIdentifier ((:=.)),
    Qualifiers,
  )

type TypeDeclaration :: Locality -> Layout -> Stage -> Environment -> Data.Kind.Type
data TypeDeclaration locality layout stage scope
  = TypeDeclaration
  { position :: !Position,
    name :: !ConstructorIdentifier,
    constructorNames :: !(Strict.Vector Constructor),
    definition :: TypeDefinition2 locality layout stage scope,
    kind :: Inferred Simple.Type stage scope
  }
  deriving (Show)

instance Shift (TypeDeclaration locality layout stage) where
  shift = shiftDefault

instance Shift.Functor (TypeDeclaration locality layout stage) where
  map category = \case
    TypeDeclaration {position, name, constructorNames, definition, kind} ->
      TypeDeclaration
        { position,
          name,
          constructorNames,
          definition = Shift.map category definition,
          kind = Shift.map category kind
        }

instance FreeTypeVariables (TypeDeclaration locality layout) where
  freeTypeVariables target = \case
    TypeDeclaration {definition} -> freeTypeVariables target definition

kind' :: TypeDeclaration locality layout Check scope -> Simple.Type scope
kind' TypeDeclaration {kind = Solved kind} = kind

lazy ::
  TypeDeclaration locality' layout' stage' scope' ->
  TypeDeclaration locality layout stage scope ->
  TypeDeclaration locality layout stage scope
lazy TypeDeclaration {position, name, constructorNames} ~TypeDeclaration {definition, kind} =
  TypeDeclaration {position, name, constructorNames, definition, kind}

labelBinding :: Qualifiers -> TypeDeclaration locality layout stage scope -> Label.TypeBinding scope'
labelBinding path declaration =
  Label.TypeBinding
    { name = path :=. name declaration,
      constructorNames = (path :=) <$> constructorNames declaration
    }

locality :: TypeDeclaration locality Normal stage scope -> TypeDeclaration locality' Normal stage scope
locality = \case
  TypeDeclaration {position, name, constructorNames, definition, kind} ->
    TypeDeclaration
      { position,
        name,
        constructorNames,
        definition = TypeDefinition2.locality definition,
        kind
      }

group ::
  Qualifiers ->
  (Type0.Index scope -> Type.Link locality) ->
  (Type.Link locality -> Groupable scope) ->
  StronglyConnected.Component (Type.Link locality) ->
  TypeDeclaration locality Normal Resolve scope ->
  TypeDeclaration locality Group Resolve scope
group qualifiers link index group = \case
  TypeDeclaration {position, name, constructorNames, definition} ->
    TypeDeclaration
      { position,
        name,
        constructorNames,
        definition = TypeDefinition2.group qualifiers link index group definition,
        kind = Inferred
      }

ungroup ::
  (Type.Link locality -> Type0.Index scope) ->
  (Type.Link locality -> TypeDefinition2.Set locality Check scope) ->
  TypeDeclaration locality Group Check scope ->
  TypeDeclaration locality Normal Check scope
ungroup index lookup TypeDeclaration {position, name, constructorNames, definition, kind} =
  TypeDeclaration
    { position,
      name,
      constructorNames,
      definition = TypeDefinition2.ungroup index lookup definition,
      kind
    }

ungroupM ::
  (Monad m) =>
  (Type.Link locality -> Type0.Index scope) ->
  (Type.Link locality -> m (TypeDefinition2.Set locality Check scope)) ->
  TypeDeclaration locality Group Check scope ->
  m (TypeDeclaration locality Normal Check scope)
ungroupM index lookup TypeDeclaration {position, name, constructorNames, definition, kind} = do
  definition <- TypeDefinition2.ungroupM index lookup definition
  pure
    TypeDeclaration
      { position,
        name,
        constructorNames,
        definition,
        kind
      }

data Groupable scope = Groupable
  { element :: !(TypeDefinition Constructive Resolve scope),
    position' :: !Position,
    name' :: !ConstructorIdentifier,
    constructorNames' :: !(Strict.Vector Constructor)
  }

groupable :: TypeDeclaration locality Normal Resolve scope -> Maybe (Groupable scope)
groupable TypeDeclaration {position, name, constructorNames, definition} = case definition of
  TypeDefinition2.InferredCyclic TypeDefinition2.::: definition ->
    Just
      Groupable
        { element = definition,
          position' = position,
          name' = name,
          constructorNames' = constructorNames
        }
  TypeDefinition2.Annotated {} TypeDefinition2.::: _ -> Nothing
  TypeDefinition2.InferredAcyclic TypeDefinition2.::: _ -> Nothing

groupFree :: Groupable scope -> [Type0.Index scope]
groupFree Groupable {element} = freeTypeVariables Target element
