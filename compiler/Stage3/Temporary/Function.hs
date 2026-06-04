module Stage3.Temporary.Function where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Layout (Group)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Pattern)
import Stage2.Shift (shift)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Function as Solved
import qualified Stage2.Tree.Function as Stage2 (Function (..))
import Stage3.Check.Context (Context)
import Stage3.Temporary.Pattern (Pattern)
import qualified Stage3.Temporary.Pattern as Pattern
import Stage3.Temporary.RightHandSide (RightHandSide)
import qualified Stage3.Temporary.RightHandSide as RightHandSide
import qualified Stage3.Unify as Unify

data Function s scope
  = Plain {rightHandSide :: !(RightHandSide s scope)}
  | Bound
      { functionPosition :: !Position,
        patternx :: !(Pattern s scope),
        function :: !(Function s (Scope.Pattern ':+ scope))
      }

check :: Context s scope -> Unify.Type s scope -> Stage2.Function Group Resolve scope -> ST s (Function s scope)
check context typex = \case
  Stage2.Plain {rightHandSide} -> Plain <$> RightHandSide.check context typex rightHandSide
  Stage2.Bound {functionPosition, patternx, function} -> do
    argument <- Unify.fresh Unify.typex
    result <- Unify.fresh Unify.typex
    Unify.unify context functionPosition typex (Unify.function argument result)
    pattern1 <- Pattern.check context argument patternx
    context <- pure $ Pattern.augment pattern1 context
    function1 <- check context (shift result) function
    pure Bound {functionPosition, patternx = pattern1, function = function1}

solve :: Function s scope -> Unify.Solve s (Solved.Function Group Check scope)
solve Plain {rightHandSide} = do
  rightHandSide <- RightHandSide.solve rightHandSide
  pure $ Solved.Plain {rightHandSide}
solve Bound {functionPosition, patternx, function} = do
  patternx <- Pattern.solve patternx
  function <- solve function
  pure $ Solved.Bound {functionPosition, patternx, function}
