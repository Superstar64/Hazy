module Stage3.Temporary.ExpressionField where

import Control.Monad.ST (ST)
import Stage2.Layout (Group)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.ExpressionField as Solved
import qualified Stage2.Tree.ExpressionField as Stage2
import Stage3.Check.Context (Context)
import {-# SOURCE #-} Stage3.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Stage3.Temporary.Expression as Expression
import qualified Stage3.Unify as Unify

data Field s scope
  = Field
  { index :: !Int,
    expression :: !(Expression s scope)
  }

check ::
  Context s scope ->
  (Int -> Unify.Type s scope) ->
  Stage2.Field Group Resolve scope ->
  ST s (Field s scope)
check context lookup Stage2.Field {index, expression} = do
  expression <- Expression.check context (lookup index) expression
  pure
    Field
      { index,
        expression
      }

solve :: Field s scope -> ST s (Solved.Field Group Check scope)
solve Field {index, expression} = do
  expression <- Expression.solve expression
  pure $ Solved.Field {index, expression}
