{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Declaration where

import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Position (Position)
import Stage1.Variable (QualifiedVariable ((:-)), Qualifiers, Variable)
import Stage2.FreeVariables (FreeTermVariables (..))
import qualified Stage2.Index.Link.Term as Term
import qualified Stage2.Index.Term0 as Term0
import qualified Stage2.Label.Binding.Term as Label
import Stage2.Layout (Group, Normal)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition2 (Inferred)
import Stage2.Tree.Definition3 (Definition3)
import Stage2.Tree.Definition4 (Definition4)
import qualified Stage2.Tree.Definition4 as Definition4
import Prelude hiding (Either (Left, Right))

data Key
  = Named !Variable
  | Unnamed !Int
  deriving (Eq, Ord, Show)

data Declaration locality layout scope
  = Declaration
  { position :: !Position,
    name :: !Key,
    definition :: !(Definition4 locality layout scope)
  }
  deriving (Show)

instance Shift (Declaration layout locality) where
  shift = shiftDefault

instance Shift.Functor (Declaration layout locality) where
  map category Declaration {position, name, definition} =
    Declaration
      { position,
        name,
        definition = Shift.map category definition
      }

instance FreeTermVariables (Declaration layout locality) where
  freeTermVariables target Declaration {definition} = freeTermVariables target definition

labelBinding :: Qualifiers -> Declaration locality layout scope -> Label.TermBinding scope'
labelBinding path declaration = case name declaration of
  Named name -> Label.TermBinding {name = path :- name}
  Unnamed _ -> Label.SharedTermBinding

locality :: Declaration locality Normal scope -> Declaration locality' Normal scope
locality = \case
  Declaration {position, name, definition} ->
    Declaration
      { position,
        name,
        definition = Definition4.locality definition
      }

group ::
  (Term0.Index scope -> Term.Link locality) ->
  (Term.Link locality -> Definition3 Inferred Normal scope) ->
  StronglyConnected.Component (Term.Link locality) ->
  Declaration locality Normal scope ->
  Declaration locality Group scope
group link index' group = \case
  Declaration {position, name, definition} ->
    Declaration
      { position,
        name,
        definition = Definition4.group link index' group definition
      }
