module Stage3.Temporary.Method where

import Control.Monad.ST (ST)
import qualified Stage2.Tree.Method as Stage2
import Stage3.Check.Context (Context)
import {-# SOURCE #-} qualified Stage3.Simple.Scheme as Simple.Scheme
import qualified Stage3.Synonym as Synonym
import Stage3.Temporary.Scheme (Scheme)
import qualified Stage3.Temporary.Scheme as Scheme
import qualified Stage3.Tree.Method as Solved

data Method s scope = Method
  { annotation :: !(Scheme s scope)
  }

check :: Context s scope -> Stage2.Method scope -> ST s (Method s scope)
check context Stage2.Method {annotation} = do
  annotation <- Scheme.check context annotation
  pure Method {annotation}

solve :: Synonym.Context s scope -> Method s scope -> ST s (Solved.Method scope)
solve context Method {annotation} = do
  annotation <- Scheme.solve context annotation
  let annotation' = Simple.Scheme.simplify annotation
  pure $ Solved.Method {annotation, annotation'}
