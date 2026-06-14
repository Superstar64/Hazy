module Semantic.Check.Temporary.Select where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import {-# SOURCE #-} Semantic.Check.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Expression as Expression
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Select as Semantic
import qualified Semantic.Unify as Unify

data Select s scope
  = Select
  { pick :: !Int,
    update :: !(Expression s scope)
  }

check :: Context s scope -> Unify.Type s scope -> Semantic.Select Group Resolve scope -> ST s (Select s scope)
check context typex Semantic.Select {pick, update} = do
  update <- Expression.check context typex update
  pure Select {pick, update}

solve :: Select s scope -> Unify.Solve s (Semantic.Select Group Check scope)
solve Select {pick, update} = do
  update <- Expression.solve update
  pure Semantic.Select {pick, update}
