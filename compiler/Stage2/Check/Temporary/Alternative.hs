module Stage2.Check.Temporary.Alternative where

import Control.Monad.ST (ST)
import Stage2.Layout (Group)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Pattern)
import Stage2.Shift (shift)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Alternative as Solved
import qualified Stage2.Tree.Alternative as Stage2
import Stage2.Check.Context (Context)
import Stage2.Check.Temporary.Pattern (Pattern)
import qualified Stage2.Check.Temporary.Pattern as Pattern
import Stage2.Check.Temporary.RightHandSide (RightHandSide)
import qualified Stage2.Check.Temporary.RightHandSide as RightHandSide
import qualified Stage2.Unify as Unify

data Alternative s scope
  = Alternative
  { parameter :: !(Pattern s scope),
    rightHandSide :: !(RightHandSide s (Scope.Pattern ':+ scope))
  }

check ::
  Context s scope ->
  Unify.Type s scope ->
  Unify.Type s scope ->
  Stage2.Alternative Group Resolve scope ->
  ST s (Alternative s scope)
check context typex binder Stage2.Alternative {parameter, rightHandSide} = do
  parameter <- Pattern.check context binder parameter
  rightHandSide <- RightHandSide.check (Pattern.augment parameter context) (shift typex) rightHandSide
  pure Alternative {parameter, rightHandSide}

solve :: Alternative s scope -> Unify.Solve s (Solved.Alternative Group Check scope)
solve Alternative {parameter, rightHandSide} = do
  parameter <- Pattern.solve parameter
  rightHandSide <- RightHandSide.solve rightHandSide
  pure Solved.Alternative {parameter, rightHandSide}
