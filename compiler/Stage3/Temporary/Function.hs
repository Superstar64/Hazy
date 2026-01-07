module Stage3.Temporary.Function where

import Control.Monad.ST (ST)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Pattern)
import Stage2.Shift (shift)
import qualified Stage2.Tree.Function as Stage2 (Function (..))
import Stage3.Check.Context (Context)
import Stage3.Temporary.Pattern (Pattern)
import qualified Stage3.Temporary.Pattern as Pattern
import Stage3.Temporary.RightHandSide (RightHandSide)
import qualified Stage3.Temporary.RightHandSide as RightHandSide
import qualified Stage3.Tree.Function as Solved
import qualified Stage3.Unify as Unify

data Function s scope
  = Plain !(RightHandSide s scope)
  | Bound !(Pattern s scope) !(Function s (Scope.Pattern ':+ scope))

check :: Context s scope -> Unify.Type s scope -> Stage2.Function scope -> ST s (Function s scope)
check context typex = \case
  Stage2.Plain {Stage2.rightHandSide} -> Plain <$> RightHandSide.check context typex rightHandSide
  Stage2.Bound {Stage2.functionPosition, Stage2.patternx, Stage2.function} -> do
    argument <- Unify.fresh Unify.typex
    result <- Unify.fresh Unify.typex
    Unify.unify context functionPosition typex (Unify.function argument result)
    pattern1 <- Pattern.check context argument patternx
    context <- pure $ Pattern.augment pattern1 context
    function1 <- check context (shift result) function
    pure (Bound pattern1 function1)

solve :: Function s scope -> ST s (Solved.Function scope)
solve (Plain rightHandSide) = do
  rightHandSide <- RightHandSide.solve rightHandSide
  pure $ Solved.Plain rightHandSide
solve (Bound patternx function) = do
  patternx <- Pattern.solve patternx
  function <- solve function
  pure $ Solved.Bound patternx function
