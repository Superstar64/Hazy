{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Declaration where

import Stage1.Position (Position)
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (QualifiedVariable ((:-)), Qualifiers, Variable)
import Stage2.FreeVariables (FreeTermVariables (..), Target (..))
import qualified Stage2.Index.Term0 as Term0
import qualified Stage2.Label.Binding.Term as Label
import Stage2.Layout (Normal)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition2 (Annotated, Inferred, Single)
import Stage2.Tree.Definition3 (Definition3)
import qualified Stage2.Tree.Definition3 as Definition3
import Stage2.Tree.Scheme (Scheme)
import Prelude hiding (Either (Left, Right))

data Declaration locality layout scope
  = Annotated
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity,
        annotation :: !(Scheme Position scope),
        definition :: !(Definition3 locality Single Annotated layout scope)
      }
  | Inferred
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity,
        definition' :: !(Definition3 locality Single Inferred layout scope)
      }
  deriving (Show)

instance Shift (Declaration layout locality) where
  shift = shiftDefault

instance Shift.Functor (Declaration layout locality) where
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

instance FreeTermVariables (Declaration layout locality) where
  freeTermVariables target = \case
    Annotated {definition} -> freeTermVariables target definition
    Inferred {definition'} -> freeTermVariables target definition'

freeGroupTermVariables :: Declaration locality layout scope -> [Term0.Index scope]
freeGroupTermVariables = \case
  Annotated {} -> []
  declaration -> freeTermVariables Target declaration

labelBinding :: Qualifiers -> Declaration locality layout scope -> Label.TermBinding scope'
labelBinding path declaration = Label.TermBinding {name = path :- name declaration}

locality :: Declaration locality Normal scope -> Declaration locality' Normal scope
locality = \case
  Annotated {position, name, fixity, annotation, definition} ->
    Annotated
      { position,
        name,
        fixity,
        annotation,
        definition = Definition3.locality definition
      }
  Inferred {position, name, fixity, definition'} ->
    Inferred
      { position,
        name,
        fixity,
        definition' = Definition3.locality definition'
      }
