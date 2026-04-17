{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Declaration where

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

data Declaration scope
  = Annotated
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity,
        annotation :: !(Scheme Position scope),
        definition :: !(Definition2 Annotated scope)
      }
  | Inferred
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity,
        definition' :: !(Definition2 Inferred scope)
      }
  deriving (Show)

instance Shift Declaration where
  shift = shiftDefault

instance Shift.Functor Declaration where
  map category = \case
    Annotated {position, name, fixity, annotation, definition} ->
      Annotated
        { position,
          name,
          fixity,
          annotation = Shift.map category annotation,
          definition = Shift.map category definition
        }
    Inferred {position, name, fixity, definition'} ->
      Inferred
        { position,
          name,
          fixity,
          definition' = Shift.map category definition'
        }

instance FreeTermVariables Declaration where
  freeTermVariables target = \case
    Annotated {definition} -> freeTermVariables target definition
    Inferred {definition'} -> freeTermVariables target definition'

labelBinding :: Qualifiers -> Declaration scope -> Label.TermBinding scope'
labelBinding path declaration = Label.TermBinding {name = path :- name declaration}
