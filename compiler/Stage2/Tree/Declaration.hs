{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Declaration where

import Stage1.Position (Position)
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (QualifiedVariable ((:-)), Qualifiers, Variable)
import Stage2.FreeVariables (FreeTermVariables (..), Target (..))
import qualified Stage2.Index.Term0 as Term0
import qualified Stage2.Label.Binding.Term as Label
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition2 (Annotated, Definition2, Inferred, Single)
import qualified Stage2.Tree.Definition2 as Definition2
import Stage2.Tree.Scheme (Scheme)
import Prelude hiding (Either (Left, Right))

data Declaration locality scope
  = Annotated
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity,
        annotation :: !(Scheme Position scope),
        definition :: !(Definition2 locality Single Annotated scope)
      }
  | Inferred
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity,
        definition' :: !(Definition2 locality Single Inferred scope)
      }
  deriving (Show)

instance Shift (Declaration locality) where
  shift = shiftDefault

instance Shift.Functor (Declaration locality) where
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

instance FreeTermVariables (Declaration locality) where
  freeTermVariables target = \case
    Annotated {definition} -> freeTermVariables target definition
    Inferred {definition'} -> freeTermVariables target definition'

freeGroupTermVariables :: Declaration locality scope -> [Term0.Index scope]
freeGroupTermVariables = \case
  Annotated {} -> []
  declaration -> freeTermVariables Target declaration

labelBinding :: Qualifiers -> Declaration locality scope -> Label.TermBinding scope'
labelBinding path declaration = Label.TermBinding {name = path :- name declaration}

locality :: Declaration locality scope -> Declaration locality' scope
locality = \case
  Annotated {position, name, fixity, annotation, definition} ->
    Annotated
      { position,
        name,
        fixity,
        annotation,
        definition = Definition2.locality definition
      }
  Inferred {position, name, fixity, definition'} ->
    Inferred
      { position,
        name,
        fixity,
        definition' = Definition2.locality definition'
      }
