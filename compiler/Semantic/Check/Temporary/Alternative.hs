module Semantic.Check.Temporary.Alternative where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import Semantic.Check.Temporary.Pattern (Pattern)
import qualified Semantic.Check.Temporary.Pattern as Pattern
import Semantic.Check.Temporary.RightHandSide (RightHandSide)
import qualified Semantic.Check.Temporary.RightHandSide as RightHandSide
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope (Pattern)
import Semantic.Shift (shift)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Alternative as Semantic
import qualified Semantic.Tree.Alternative as Solved
import qualified Semantic.Unify as Unify

data Alternative s scope
  = Alternative
  { parameter :: !(Pattern s scope),
    rightHandSide :: !(RightHandSide s (Scope.Pattern ':+ scope))
  }

check ::
  Context s scope ->
  Unify.Type s scope ->
  Unify.Type s scope ->
  Semantic.Alternative Group Resolve scope ->
  ST s (Alternative s scope)
check context typex binder Semantic.Alternative {parameter, rightHandSide} = do
  parameter <- Pattern.check context binder parameter
  rightHandSide <- RightHandSide.check (Pattern.augment parameter context) (shift typex) rightHandSide
  pure Alternative {parameter, rightHandSide}

solve :: Alternative s scope -> Unify.Solve s (Solved.Alternative Group Check scope)
solve Alternative {parameter, rightHandSide} = do
  parameter <- Pattern.solve parameter
  rightHandSide <- RightHandSide.solve rightHandSide
  pure Solved.Alternative {parameter, rightHandSide}
