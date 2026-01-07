{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.TypeDeclaration
  ( TypeDeclaration (..),
    labelBinding,
  )
where

import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import Stage1.Tree.Brand (Brand)
import Stage1.Variable
  ( ConstructorIdentifier,
    QualifiedConstructor ((:=)),
    QualifiedConstructorIdentifier ((:=.)),
    Qualifiers,
  )
import qualified Stage2.Label.Binding.Type as Label
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Constraint (Constraint)
import Stage2.Tree.Constructor (Constructor)
import qualified Stage2.Tree.Constructor as Constructor
import Stage2.Tree.GADTConstructor (GADTConstructor)
import qualified Stage2.Tree.GADTConstructor as GADTConstructor
import Stage2.Tree.Method (Method)
import Stage2.Tree.Selector (Selector)
import Stage2.Tree.Type (Type)
import Stage2.Tree.TypePattern (TypePattern)

data TypeDeclaration scope
  = ADT
      { position :: !Position,
        name :: !ConstructorIdentifier,
        brand :: !Brand,
        parameters :: !(Strict.Vector (TypePattern Position)),
        constructors :: !(Strict.Vector (Constructor (Local ':+ scope))),
        selectors :: !(Strict.Vector (Selector)),
        annotation :: !(Strict.Maybe (Type Position scope))
      }
  | GADT
      { position :: !Position,
        name :: !ConstructorIdentifier,
        brand :: !Brand,
        parameters :: !(Strict.Vector (TypePattern Position)),
        gadtConstructors :: !(Strict.Vector (GADTConstructor scope)),
        annotation :: !(Strict.Maybe (Type Position scope))
      }
  | Class
      { position :: !Position,
        name :: !ConstructorIdentifier,
        parameter :: !(TypePattern Position),
        methods :: !(Strict.Vector (Method (Local ':+ scope))),
        constraints :: !(Strict.Vector (Constraint Position scope)),
        annotation :: !(Strict.Maybe (Type Position scope))
      }
  | Synonym
      { position :: !Position,
        name :: !ConstructorIdentifier,
        parameters :: !(Strict.Vector (TypePattern Position)),
        synonym :: !(Type Position (Local ':+ scope)),
        annotation :: !(Strict.Maybe (Type Position scope))
      }
  deriving (Show)

instance Shift TypeDeclaration where
  shift = shiftDefault

instance Shift.Functor TypeDeclaration where
  map category = \case
    ADT {position, name, brand, parameters, constructors, selectors, annotation} ->
      ADT
        { position,
          name,
          brand,
          parameters,
          constructors = fmap (Shift.map (Shift.Over category)) constructors,
          selectors,
          annotation = fmap (Shift.map category) annotation
        }
    GADT {position, name, brand, parameters, gadtConstructors, annotation} ->
      GADT
        { position,
          name,
          brand,
          parameters,
          gadtConstructors = fmap (Shift.map category) gadtConstructors,
          annotation = fmap (Shift.map category) annotation
        }
    Class {position, name, parameter, methods, constraints, annotation} ->
      Class
        { position,
          name,
          parameter,
          constraints = fmap (Shift.map category) constraints,
          methods = fmap (Shift.map (Shift.Over category)) methods,
          annotation = fmap (Shift.map category) annotation
        }
    Synonym {position, name, parameters, synonym, annotation} ->
      Synonym
        { position,
          name,
          parameters,
          synonym = Shift.map (Shift.Over category) synonym,
          annotation = fmap (Shift.map category) annotation
        }

labelBinding :: Qualifiers -> TypeDeclaration scope -> Label.TypeBinding scope'
labelBinding path declaration = Label.TypeBinding {Label.name = path :=. name declaration, Label.constructorNames}
  where
    constructorNames = case declaration of
      ADT {constructors} -> (:=) path . Constructor.name <$> constructors
      GADT {gadtConstructors} -> (:=) path . GADTConstructor.name <$> gadtConstructors
      _ -> Strict.Vector.empty
