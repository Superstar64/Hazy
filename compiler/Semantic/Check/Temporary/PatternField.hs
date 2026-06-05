module Semantic.Check.Temporary.PatternField where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import {-# SOURCE #-} Semantic.Check.Temporary.Pattern (Pattern)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Pattern as Pattern
import Semantic.Check.TermBinding (TermBinding)
import qualified Semantic.Index.Table.Term as Term
import Semantic.Index.Term (Bound)
import Semantic.Scope (Environment ((:+)))
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.PatternField as Semantic
import qualified Semantic.Tree.PatternField as Solved
import qualified Semantic.Unify as Unify

data Field s scope = Field
  { index :: !Int,
    patternx :: !(Pattern s scope)
  }

(!) :: Field s scope -> Bound -> Unify.Type s scope
Field {patternx} ! bound = patternx Pattern.! bound

augmentField :: Field s scopes -> Term.Bound (TermBinding s) (scope ':+ scopes)
augmentField Field {patternx} = Pattern.augmentPattern patternx

check :: Context s scope -> (Int -> Unify.Type s scope) -> Semantic.Field Resolve scope -> ST s (Field s scope)
check context lookup Semantic.Field {index, patternx} = do
  patternx <- Pattern.check context (lookup index) patternx
  pure
    Field
      { index,
        patternx
      }

solve :: Field s scope -> Unify.Solve s (Solved.Field Check scope)
solve Field {index, patternx} = do
  patternx <- Pattern.solve patternx
  pure $ Solved.Field index patternx
