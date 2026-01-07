module Stage3.Temporary.ExpressionField where

import Control.Monad.ST (ST)
import qualified Stage2.Tree.ExpressionField as Stage2
import Stage3.Check.Context (Context)
import {-# SOURCE #-} Stage3.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Stage3.Temporary.Expression as Expression
import qualified Stage3.Tree.ExpressionField as Solved
import qualified Stage3.Unify as Unify

data Field s scope
  = Field
  { index :: !Int,
    expression :: !(Expression s scope)
  }

check ::
  Context s scope ->
  (Int -> Unify.Type s scope) ->
  Stage2.Field scope ->
  ST s (Field s scope)
check context lookup Stage2.Field {Stage2.index, Stage2.expression} = do
  expression <- Expression.check context (lookup index) expression
  pure
    Field
      { index,
        expression
      }

solve :: Field s scope -> ST s (Solved.Field scope)
solve Field {index, expression} = do
  expression <- Expression.solve expression
  pure $ Solved.Field {Solved.index, Solved.expression}
