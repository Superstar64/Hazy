{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.TypeDeclaration
  ( TypeDeclaration (..),
    kind',
    lazy,
    labelBinding,
    locality,
    group,
    ungroup,
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
import Stage2.Scope (Environment (..))
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check, Resolve, Stage)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import Stage2.Tree.TypeDefinition (TypeDefinition)
import Stage2.Tree.TypeDefinition2 (TypeDefinition2)
import qualified Stage2.Tree.TypeDefinition2 as TypeDefinition2
import qualified Stage4.Tree.Type as Simple (Type)

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
  (Type0.Index scope -> Type.Link locality) ->
  (Type.Link locality -> TypeDefinition Resolve scope) ->
  StronglyConnected.Component (Type.Link locality) ->
  TypeDeclaration locality Normal Resolve scope ->
  TypeDeclaration locality Group Resolve scope
group link index group = \case
  TypeDeclaration {position, name, constructorNames, definition} ->
    TypeDeclaration
      { position,
        name,
        constructorNames,
        definition = TypeDefinition2.group link index group definition,
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
