module Stage3.Temporary.PatternField where

import Control.Monad.ST (ST)
import qualified Stage2.Index.Table.Term as Term
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Tree.PatternField as Stage2
import Stage3.Check.Context (Context)
import Stage3.Check.TermBinding (TermBinding)
import {-# SOURCE #-} Stage3.Temporary.Pattern (Pattern)
import {-# SOURCE #-} qualified Stage3.Temporary.Pattern as Pattern
import qualified Stage3.Tree.PatternField as Solved
import qualified Stage3.Unify as Unify

data Field s scope = Field
  { index :: !Int,
    patternx :: !(Pattern s scope)
  }

augmentField :: Field s scopes -> Term.Bound (TermBinding s) (scope ':+ scopes)
augmentField Field {patternx} = Pattern.augmentPattern patternx

check :: Context s scope -> (Int -> Unify.Type s scope) -> Stage2.Field scope -> ST s (Field s scope)
check context lookup Stage2.Field {index, patternx} = do
  patternx <- Pattern.check context (lookup index) patternx
  pure
    Field
      { index,
        patternx
      }

solve :: Field s scope -> ST s (Solved.Field scope)
solve Field {index, patternx} = do
  patternx <- Pattern.solve patternx
  pure $ Solved.Field index patternx
