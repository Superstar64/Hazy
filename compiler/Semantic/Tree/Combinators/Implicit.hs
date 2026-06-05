module Semantic.Tree.Combinators.Implicit where

import qualified Core.Tree.SchemeOver as Simple
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve)
import Prelude hiding (map)

-- It would probably be cleaner if `ast` was also parameterized by a stage, but
-- that would require quanified constraints for implementing the instances

data Implicit ast stage scope where
  Resolve :: !(ast scope) -> Implicit ast Resolve scope
  Check :: !(Simple.SchemeOver ast scope) -> Implicit ast Check scope

instance (Scope.Show ast) => Show (Implicit ast stage scope) where
  showsPrec d = \case
    Resolve ast -> showParen (d > 10) $ showString "Resolve " . Scope.showsPrec 11 ast
    Check ast -> showParen (d > 10) $ showString "Check " . showsPrec 11 ast

instance (Shift.Functor ast) => Shift (Implicit ast stace) where
  shift = shiftDefault

instance (Shift.Functor ast) => Shift.Functor (Implicit ast stage) where
  map category = \case
    Resolve ast -> Resolve (Shift.map category ast)
    Check ast -> Check (Shift.map category ast)
