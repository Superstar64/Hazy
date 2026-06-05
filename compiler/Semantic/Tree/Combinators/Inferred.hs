module Semantic.Tree.Combinators.Inferred where

import Data.Kind (Type)
import Semantic.Scope (Environment)
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift (shift))
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve, Stage)

type Inferred :: (Environment -> Type) -> Stage -> Environment -> Type
data Inferred simple stage scope where
  Inferred :: Inferred simple Resolve scope
  Solved :: !(simple scope) -> Inferred simple Check scope

instance (Scope.Show simple) => Show (Inferred simple stage scope) where
  showsPrec d = \case
    Inferred -> showString "Inferred"
    Solved typex -> showParen (d > 10) $ showString "Solved " . Scope.showsPrec 11 typex

instance (Shift simple) => Shift (Inferred simple stage) where
  shift = \case
    Inferred -> Inferred
    Solved typex -> Solved (shift typex)

instance (Shift.Functor simple) => Shift.Functor (Inferred simple stage) where
  map category = \case
    Inferred -> Inferred
    Solved typex -> Solved (Shift.map category typex)

get :: Inferred simple Check scope -> simple scope
get (Solved typex) = typex
