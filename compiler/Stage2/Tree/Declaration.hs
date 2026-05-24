{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Declaration where

import qualified Data.Vector.Strict as Strict
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
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import qualified Stage2.Tree.Definition2 as Definition2
import Stage2.Tree.Definition3 (Definition3)
import Stage2.Tree.Definition4 (Definition4)
import qualified Stage2.Tree.Definition4 as Definition4
import {-# SOURCE #-} qualified Stage4.Tree.Scheme as Simple (Scheme)
import Prelude hiding (Either (Left, Right))

data Key
  = Named !Variable
  | Unnamed !Int
  deriving (Eq, Ord, Show)

data Declaration locality layout stage scope
  = Declaration
  { position :: !Position,
    name :: !Key,
    definition :: Definition4 locality layout stage scope,
    typex :: Inferred Simple.Scheme stage scope
  }
  deriving (Show)

lazy ::
  Declaration locality' layout' stage' scope' ->
  Declaration locality layout stage scope ->
  Declaration locality layout stage scope
lazy Declaration {position, name} ~Declaration {definition, typex} =
  Declaration
    { position,
      name,
      definition,
      typex
    }

typex' :: Declaration locality layout Check scope -> Simple.Scheme scope
typex' Declaration {typex = Solved typex} = typex

instance Shift (Declaration layout locality stage) where
  shift = shiftDefault

instance Shift.Functor (Declaration layout locality stage) where
  map category Declaration {position, name, definition, typex} =
    Declaration
      { position,
        name,
        definition = Shift.map category definition,
        typex = Shift.map category typex
      }

instance FreeTermVariables (Declaration layout locality) where
  freeTermVariables target Declaration {definition} = freeTermVariables target definition

labelBinding :: Qualifiers -> Declaration locality layout stage scope -> Label.TermBinding scope'
labelBinding path declaration = case name declaration of
  Named name -> Label.TermBinding {name = path :- name}
  Unnamed _ -> Label.SharedTermBinding

locality :: Declaration locality Normal stage scope -> Declaration locality' Normal stage scope
locality = \case
  Declaration {position, name, definition, typex} ->
    Declaration
      { position,
        name,
        definition = Definition4.locality definition,
        typex
      }

group ::
  (Term0.Index scope -> Term.Link locality) ->
  (Term.Link locality -> Definition3 Definition2.Inferred Normal Resolve scope) ->
  StronglyConnected.Component (Term.Link locality) ->
  Declaration locality Normal Resolve scope ->
  Declaration locality Group Resolve scope
group link index' group = \case
  Declaration {position, name, definition} ->
    Declaration
      { position,
        name,
        definition = Definition4.group link index' group definition,
        typex = Inferred
      }

ungroup ::
  (Term.Link locality -> Term0.Index scope) ->
  (Term.Link locality -> Strict.Vector (Definition4.Element locality Check scope)) ->
  Declaration locality Group Check scope ->
  Declaration locality Normal Check scope
ungroup index lookup Declaration {position, name, definition, typex} =
  Declaration
    { position,
      name,
      definition = Definition4.ungroup index lookup definition,
      typex
    }
