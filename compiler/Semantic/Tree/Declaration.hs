{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Declaration where

import qualified Core.Tree.Forall as Simple (ForallOver)
import qualified Core.Tree.Type as Simple (TypeF)
import Data.Functor.Classes (Show1, showsPrec1)
import Data.Functor.Identity (Identity (..))
import Data.Void (Void)
import qualified Graph.StronglyConnected as StronglyConnected
import Semantic.FreeVariables (FreeTermVariables (..), Target (Target))
import qualified Semantic.Index.Link.Term as Term
import qualified Semantic.Index.Term0 as Term0
import qualified Semantic.Label.Binding.Term as Label
import Semantic.Layout (Group, Normal)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Implicit (Implicit)
import qualified Semantic.Tree.Combinators.Implicit as Implicit
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Definition2 as Definition2
import Semantic.Tree.Definition3 (Definition3)
import Semantic.Tree.Definition4 (Definition4)
import qualified Semantic.Tree.Definition4 as Definition4
import qualified Semantic.Tree.Group as Group
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.Variable (QualifiedVariable ((:-)), Qualifiers, Variable)
import Prelude hiding (Either (Left, Right))

data Key
  = Named !Variable
  | Unnamed !Int
  deriving (Eq, Ord, Show)

data Declaration solve logical loeb locality layout stage scope
  = Declaration
  { position :: !Position,
    name :: !Key,
    definition :: Definition4 solve logical loeb locality layout stage scope,
    typex :: loeb (Inferred (Simple.ForallOver Simple.TypeF logical) stage scope)
  }

instance
  (Show1 solve, Show logical, Show1 loeb) =>
  Show (Declaration solve logical loeb locality layout stage scope)
  where
  showsPrec p Declaration {position, name, definition, typex} =
    showParen (p > 10) $
      showString "Declaration {position = "
        . showsPrec 0 position
        . showString ", name = "
        . showsPrec 0 name
        . showString ", definition = "
        . showsPrec 0 definition
        . showString ", typex = "
        . showsPrec1 0 typex
        . showString "}"

lazy ::
  Declaration solve' logical' loeb' locality' layout' stage' scope' ->
  Declaration solve logical loeb locality layout stage scope ->
  Declaration solve logical loeb locality layout stage scope
lazy Declaration {position, name} ~Declaration {definition, typex} =
  Declaration
    { position,
      name,
      definition,
      typex
    }

typex' :: Declaration solve logical Identity locality layout Check scope -> Simple.ForallOver Simple.TypeF logical scope
typex' Declaration {typex = Identity (Solved typex)} = typex

instance (Functor loeb, Functor solve) => Shift0.Functor (Declaration solve logical loeb layout locality stage) where
  map = Shift.mapDefault

instance (Functor loeb, Functor solve) => Shift.Functor (Declaration solve logical loeb layout locality stage) where
  map category Declaration {position, name, definition, typex} =
    Declaration
      { position,
        name,
        definition = Shift.map category definition,
        typex = fmap (Shift.map category) typex
      }

instance (Foldable loeb, Foldable solve) => FreeTermVariables (Declaration solve logical loeb locality) where
  freeTermVariables target Declaration {definition} = freeTermVariables target definition

labelBinding :: Qualifiers -> Declaration solve logical loeb locality layout stage scope -> Label.TermBinding scope'
labelBinding path declaration = case name declaration of
  Named name -> Label.TermBinding {name = path :- name}
  Unnamed _ -> Label.SharedTermBinding

locality ::
  Declaration solve logical Identity locality Normal stage scope ->
  Declaration solve logical Identity locality' Normal stage scope
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
  Declaration Identity logical Identity locality Normal Resolve scope ->
  Declaration Identity logical Identity locality Group Resolve scope
group link index' group = \case
  Declaration {position, name, definition} ->
    Declaration
      { position,
        name,
        definition = Definition4.group link index' group definition,
        typex = pure Inferred
      }

ungroup ::
  (Term.Link locality -> Term0.Index scope) ->
  (Term.Link locality -> Implicit (Group.Set locality) Group Check scope) ->
  Declaration Identity logical Identity locality Group Check scope ->
  Declaration Identity logical Identity locality Normal Check scope
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

groupable :: Declaration Identity logical Identity locality Normal Resolve scope -> Maybe (Groupable scope)
groupable Declaration {definition} = case definition of
  Definition4.Inferred Definition4.::: Identity (Identity (Implicit.Resolve definition)) ->
    Just Groupable {element = definition}
  Definition4.Annotated {} Definition4.::: _ -> Nothing

groupFree :: Groupable scope -> [Term0.Index scope]
groupFree Groupable {element} = freeTermVariables Target element

solve ::
  Declaration (Unify.Solve s) (Unify.Logical s scope) Identity locality Group Check scope ->
  Unify.Solve s (Declaration Identity Void Identity locality Group Check scope)
solve Declaration {position, name, definition, typex = Identity (Solved typex)} = do
  definition <- Definition4.solve position definition
  typex <- Unify.solve position typex
  pure
    Declaration
      { position,
        name,
        definition,
        typex = Identity (Solved typex)
      }
