{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Declaration where

import {-# SOURCE #-} qualified Core.Tree.Scheme as Simple (Scheme)
import qualified Graph.StronglyConnected as StronglyConnected
import Semantic.FreeVariables (FreeTermVariables (..), Target (Target))
import qualified Semantic.Index.Link.Term as Term
import qualified Semantic.Index.Term0 as Term0
import qualified Semantic.Label.Binding.Term as Label
import Semantic.Layout (Group, Normal)
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Implicit (Implicit)
import qualified Semantic.Tree.Combinators.Implicit as Implicit
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Definition2 as Definition2
import Semantic.Tree.Definition3 (Definition3)
import Semantic.Tree.Definition4 (Definition4)
import qualified Semantic.Tree.Definition4 as Definition4
import Syntax.Position (Position)
import Syntax.Variable (QualifiedVariable ((:-)), Qualifiers, Variable)
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
  (Term.Link locality -> Groupable scope) ->
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
  (Term.Link locality -> Implicit (Definition4.Set locality Check) Check scope) ->
  Declaration locality Group Check scope ->
  Declaration locality Normal Check scope
ungroup index lookup Declaration {position, name, definition, typex} =
  Declaration
    { position,
      name,
      definition = Definition4.ungroup index lookup definition,
      typex
    }

newtype Groupable scope
  = Groupable
  { element :: Definition3 Definition2.Inferred Normal Resolve scope
  }

groupable :: Declaration locality Normal Resolve scope -> Maybe (Groupable scope)
groupable Declaration {definition} = case definition of
  Definition4.Inferred Definition4.::: Implicit.Resolve definition ->
    Just Groupable {element = definition}
  Definition4.Annotated {} Definition4.::: _ -> Nothing

groupFree :: Groupable scope -> [Term0.Index scope]
groupFree Groupable {element} = freeTermVariables Target element
