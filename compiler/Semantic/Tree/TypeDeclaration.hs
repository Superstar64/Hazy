{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.TypeDeclaration where

import qualified Core.Tree.Type as Simple (Type)
import Data.Functor.Identity (Identity (..))
import qualified Data.Kind as Kind
import qualified Data.Vector.Strict as Strict
import qualified Graph.StronglyConnected as StronglyConnected
import Semantic.FreeVariables (FreeTypeVariables (..), Target (..))
import qualified Semantic.Index.Link.Type as Type
import qualified Semantic.Index.Type0 as Type0
import qualified Semantic.Label.Binding.Type as Label
import Semantic.Layout (Group, Layout, Normal)
import Semantic.Locality (Locality)
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check, Resolve, Stage)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.TypeDefinition (TypeDefinition)
import Semantic.Tree.TypeDefinition2 (TypeDefinition2)
import qualified Semantic.Tree.TypeDefinition2 as TypeDefinition2
import qualified Semantic.Tree.TypeGroup as TypeGroup
import Syntax.Position (Position)
import Syntax.Variable
  ( Constructor,
    ConstructorIdentifier,
    QualifiedConstructor ((:=)),
    QualifiedConstructorIdentifier ((:=.)),
    Qualifiers,
  )

type TypeDeclaration :: (Kind.Type -> Kind.Type) -> Locality -> Layout -> Stage -> Environment -> Kind.Type
data TypeDeclaration loeb locality layout stage scope
  = TypeDeclaration
  { position :: !Position,
    name :: !ConstructorIdentifier,
    constructorNames :: !(Strict.Vector Constructor),
    definition :: TypeDefinition2 loeb locality layout stage scope,
    kind :: loeb (Inferred Simple.Type stage scope)
  }
  deriving (Show)

instance (Functor loeb) => Shift0.Functor (TypeDeclaration loeb locality layout stage) where
  map = Shift.mapDefault

instance (Functor loeb) => Shift.Functor (TypeDeclaration loeb locality layout stage) where
  map category = \case
    TypeDeclaration {position, name, constructorNames, definition, kind} ->
      TypeDeclaration
        { position,
          name,
          constructorNames,
          definition = Shift.map category definition,
          kind = Shift.map category <$> kind
        }

instance (Foldable loeb) => FreeTypeVariables (TypeDeclaration loeb locality layout) where
  freeTypeVariables target = \case
    TypeDeclaration {definition} -> freeTypeVariables target definition

kind' :: TypeDeclaration Identity locality layout Check scope -> Simple.Type scope
kind' TypeDeclaration {kind = Identity (Solved kind)} = kind

lazy ::
  TypeDeclaration loeb locality' layout' stage' scope' ->
  TypeDeclaration loeb locality layout stage scope ->
  TypeDeclaration loeb locality layout stage scope
lazy TypeDeclaration {position, name, constructorNames} ~TypeDeclaration {definition, kind} =
  TypeDeclaration {position, name, constructorNames, definition, kind}

labelBinding :: Qualifiers -> TypeDeclaration loeb locality layout stage scope -> Label.TypeBinding scope'
labelBinding path declaration =
  Label.TypeBinding
    { name = path :=. name declaration,
      constructorNames = (path :=) <$> constructorNames declaration
    }

locality :: TypeDeclaration loeb locality Normal stage scope -> TypeDeclaration loeb locality' Normal stage scope
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
  TypeDeclaration Identity locality Normal Resolve scope ->
  TypeDeclaration Identity locality Group Resolve scope
group qualifiers link index group = \case
  TypeDeclaration {position, name, constructorNames, definition} ->
    TypeDeclaration
      { position,
        name,
        constructorNames,
        definition = TypeDefinition2.group qualifiers link index group definition,
        kind = pure Inferred
      }

ungroup ::
  (Type.Link locality -> Type0.Index scope) ->
  (Type.Link locality -> TypeGroup.Set locality Check scope) ->
  TypeDeclaration Identity locality Group Check scope ->
  TypeDeclaration Identity locality Normal Check scope
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
  (Type.Link locality -> m (TypeGroup.Set locality Check scope)) ->
  TypeDeclaration Identity locality Group Check scope ->
  m (TypeDeclaration Identity locality Normal Check scope)
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
  { element :: !(TypeDefinition Resolve scope),
    position' :: !Position,
    name' :: !ConstructorIdentifier,
    constructorNames' :: !(Strict.Vector Constructor)
  }

groupable :: TypeDeclaration Identity locality Normal Resolve scope -> Maybe (Groupable scope)
groupable TypeDeclaration {position, name, constructorNames, definition} = case definition of
  TypeDefinition2.Inferred TypeDefinition2.::: Identity definition ->
    Just
      Groupable
        { element = definition,
          position' = position,
          name' = name,
          constructorNames' = constructorNames
        }
  TypeDefinition2.Annotated {} TypeDefinition2.::: _ -> Nothing
  TypeDefinition2.Synonym _ -> Nothing

groupFree :: Groupable scope -> [Type0.Index scope]
groupFree Groupable {element} = freeTypeVariables Target element
