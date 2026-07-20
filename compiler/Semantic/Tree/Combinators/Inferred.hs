module Semantic.Tree.Combinators.Inferred where

import Data.Kind (Type)
import Semantic.Scope (Environment)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check, Resolve, Stage)

type Inferred :: (Environment -> Type) -> Stage -> Environment -> Type
data Inferred simple stage scope where
  Inferred :: Inferred simple Resolve scope
  Solved :: !(simple scope) -> Inferred simple Check scope

instance (Scope.Show simple) => Show (Inferred simple stage scope) where
  showsPrec d = \case
    Inferred -> showString "Inferred"
    Solved typex -> showParen (d > 10) $ showString "Solved " . Scope.showsPrec 11 typex

instance (Shift0.Functor simple) => Shift0.Functor (Inferred simple stage) where
  map category = \case
    Inferred -> Inferred
    Solved typex -> Solved (Shift0.map category typex)

instance (Shift.Functor simple) => Shift.Functor (Inferred simple stage) where
  map category = \case
    Inferred -> Inferred
    Solved typex -> Solved (Shift.map category typex)

get :: Inferred simple Check scope -> simple scope
get (Solved typex) = typex
