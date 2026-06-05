module Semantic.Check.Temporary.ExpressionField where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import {-# SOURCE #-} Semantic.Check.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Expression as Expression
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.ExpressionField as Semantic
import qualified Semantic.Tree.ExpressionField as Solved
import qualified Semantic.Unify as Unify

data Field s scope
  = Field
  { index :: !Int,
    expression :: !(Expression s scope)
  }

check ::
  Context s scope ->
  (Int -> Unify.Type s scope) ->
  Semantic.Field Group Resolve scope ->
  ST s (Field s scope)
check context lookup Semantic.Field {index, expression} = do
  expression <- Expression.check context (lookup index) expression
  pure
    Field
      { index,
        expression
      }

solve :: Field s scope -> Unify.Solve s (Solved.Field Group Check scope)
solve Field {index, expression} = do
  expression <- Expression.solve expression
  pure $ Solved.Field {index, expression}
