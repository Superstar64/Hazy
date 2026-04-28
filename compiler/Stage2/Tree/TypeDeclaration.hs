{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.TypeDeclaration
  ( TypeDeclaration (..),
    freeGroupTypeVariables,
    labelBinding,
  )
where

import qualified Data.Vector.Strict as Strict
import Stage1.Position (Position)
import Stage1.Variable
  ( Constructor,
    ConstructorIdentifier,
    QualifiedConstructor ((:=)),
    QualifiedConstructorIdentifier ((:=.)),
    Qualifiers,
  )
import Stage2.FreeVariables (FreeTypeVariables (..), Target (..))
import qualified Stage2.Index.Type0 as Type0
import qualified Stage2.Label.Binding.Type as Label
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Type (Type)
import Stage2.Tree.TypeDefinition (TypeDefinition)

data TypeDeclaration scope
  = Annotated
      { position :: !Position,
        name :: !ConstructorIdentifier,
        constructorNames :: !(Strict.Vector Constructor),
        annotation :: !(Type Position scope),
        definition :: !(TypeDefinition scope)
      }
  | Inferred
      { position :: !Position,
        name :: !ConstructorIdentifier,
        constructorNames :: !(Strict.Vector Constructor),
        definition :: !(TypeDefinition scope)
      }
  deriving (Show)

instance Shift TypeDeclaration where
  shift = shiftDefault

instance Shift.Functor TypeDeclaration where
  map category = \case
    Annotated {position, name, constructorNames, annotation, definition} ->
      Annotated
        { position,
          name,
          constructorNames,
          annotation = Shift.map category annotation,
          definition = Shift.map category definition
        }
    Inferred {position, name, constructorNames, definition} ->
      Inferred
        { position,
          name,
          constructorNames,
          definition = Shift.map category definition
        }

instance FreeTypeVariables TypeDeclaration where
  freeTypeVariables target = \case
    Annotated {annotation, definition} ->
      concat
        [ freeTypeVariables target annotation,
          freeTypeVariables target definition
        ]
    Inferred {definition} -> freeTypeVariables target definition

freeGroupTypeVariables :: TypeDeclaration scope -> [Type0.Index scope]
freeGroupTypeVariables = \case
  Annotated {} -> []
  declaration -> freeTypeVariables Target declaration

labelBinding :: Qualifiers -> TypeDeclaration scope -> Label.TypeBinding scope'
labelBinding path declaration =
  Label.TypeBinding
    { name = path :=. name declaration,
      constructorNames = (path :=) <$> constructorNames declaration
    }
