-- | Wrapper to redirect type classes
module Semantic.Term where

import Data.Kind (Type)
import Semantic.Layout (Layout)
import Semantic.Scope (Environment)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import qualified Semantic.Show as Term
import Semantic.Stage (Stage)

type Term :: (Layout -> Stage -> Environment -> Type) -> Layout -> Stage -> Environment -> Type
newtype Term ast layout stage scope = Term {runTerm :: ast layout stage scope}

instance (Term.Show ast) => Scope.Show (Term ast layout stage) where
  showsPrec k (Term term) = Term.showsPrec k term

instance (Shift0.TermFunctor ast) => Shift0.Functor (Term ast layout stage) where
  map category (Term term) = Term $ Shift0.mapTerm category term

instance (Shift.TermFunctor ast) => Shift.Functor (Term ast layout stage) where
  map category (Term term) = Term $ Shift.mapTerm category term
