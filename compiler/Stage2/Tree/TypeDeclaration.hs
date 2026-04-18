{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.TypeDeclaration
  ( TypeDeclaration (..),
    freeGroupTypeVariables,
    labelBinding,
  )
where

import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import Stage1.Variable
  ( ConstructorIdentifier,
    QualifiedConstructor ((:=)),
    QualifiedConstructorIdentifier ((:=.)),
    Qualifiers,
  )
import Stage2.FreeVariables (FreeTypeVariables (..), Target (..))
import qualified Stage2.Index.Type0 as Type0
import qualified Stage2.Label.Binding.Type as Label
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage2.Tree.Constructor as Constructor
import qualified Stage2.Tree.GADTConstructor as GADTConstructor
import Stage2.Tree.Type (Type)
import Stage2.Tree.TypeDefinition (TypeDefinition (..))

data TypeDeclaration scope
  = TypeDeclaration
  { position :: !Position,
    name :: !ConstructorIdentifier,
    annotation :: !(Strict.Maybe (Type Position scope)),
    definition :: !(TypeDefinition scope)
  }
  deriving (Show)

instance Shift TypeDeclaration where
  shift = shiftDefault

instance Shift.Functor TypeDeclaration where
  map category = \case
    TypeDeclaration {position, name, annotation, definition} ->
      TypeDeclaration
        { position,
          name,
          annotation = Shift.map category <$> annotation,
          definition = Shift.map category definition
        }

instance FreeTypeVariables TypeDeclaration where
  freeTypeVariables target = \case
    TypeDeclaration {annotation, definition} ->
      concat
        [ foldMap (freeTypeVariables target) annotation,
          freeTypeVariables target definition
        ]

freeGroupTypeVariables :: TypeDeclaration scope -> [Type0.Index scope]
freeGroupTypeVariables = \case
  TypeDeclaration {annotation = Strict.Just {}} -> []
  declaration -> freeTypeVariables Target declaration

labelBinding :: Qualifiers -> TypeDeclaration scope -> Label.TypeBinding scope'
labelBinding path TypeDeclaration {name, definition} = Label.TypeBinding {name = path :=. name, constructorNames}
  where
    constructorNames = case definition of
      ADT {constructors} -> (:=) path . Constructor.name <$> constructors
      GADT {gadtConstructors} -> (:=) path . GADTConstructor.name <$> gadtConstructors
      _ -> Strict.Vector.empty
