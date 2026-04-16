{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.TermDeclaration where

import Stage1.Position (Position)
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (QualifiedVariable ((:-)), Qualifiers, Variable)
import Stage2.FreeVariables (FreeTermVariables (..))
import qualified Stage2.Label.Binding.Term as Label
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition2 (Annotated, Definition2, Inferred)
import Stage2.Tree.Scheme (Scheme)
import Prelude hiding (Either (Left, Right))

data TermDeclaration scope
  = TermDeclaration
  { position :: !Position,
    name :: !Variable,
    fixity :: !Fixity,
    declaration :: !(TermDeclaration' scope)
  }
  deriving (Show)

instance Shift TermDeclaration where
  shift = shiftDefault

instance Shift.Functor TermDeclaration where
  map category = \case
    TermDeclaration {position, name, fixity, declaration} ->
      TermDeclaration
        { position,
          name,
          fixity,
          declaration = Shift.map category declaration
        }

instance FreeTermVariables TermDeclaration where
  freeTermVariables target TermDeclaration {declaration} =
    freeTermVariables target declaration

data TermDeclaration' scope
  = Annotated
      { annotation :: !(Scheme Position scope),
        definition :: !(Definition2 Annotated scope)
      }
  | Inferred
      { definition' :: !(Definition2 Inferred scope)
      }
  deriving (Show)

instance Shift TermDeclaration' where
  shift = shiftDefault

instance Shift.Functor TermDeclaration' where
  map category = \case
    Annotated {annotation, definition} ->
      Annotated
        { annotation = Shift.map category annotation,
          definition = Shift.map category definition
        }
    Inferred {definition'} ->
      Inferred
        { definition' = Shift.map category definition'
        }

instance FreeTermVariables TermDeclaration' where
  freeTermVariables target = \case
    Annotated {definition} -> freeTermVariables target definition
    Inferred {definition'} -> freeTermVariables target definition'

labelBinding :: Qualifiers -> TermDeclaration scope -> Label.TermBinding scope'
labelBinding path declaration = Label.TermBinding {name = path :- name declaration}
