module Semantic.Check.Temporary.Function where

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
import qualified Semantic.Tree.Function as Semantic (Function (..))
import qualified Semantic.Tree.Function as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Function s scope
  = Plain {rightHandSide :: !(RightHandSide s scope)}
  | Bound
      { functionPosition :: !Position,
        patternx :: !(Pattern s scope),
        function :: !(Function s (Scope.Pattern ':+ scope))
      }

check :: Context s scope -> Unify.Type s scope -> Semantic.Function Group Resolve scope -> ST s (Function s scope)
check context typex = \case
  Semantic.Plain {rightHandSide} -> Plain <$> RightHandSide.check context typex rightHandSide
  Semantic.Bound {functionPosition, patternx, function} -> do
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
