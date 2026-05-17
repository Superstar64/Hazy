module Stage3.Temporary.Method where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import qualified Stage2.Tree.Method as Stage2 (Method (..))
import Stage3.Check.Context (Context)
import Stage3.Temporary.Scheme (Scheme)
import qualified Stage3.Temporary.Scheme as Scheme
import qualified Stage3.Tree.Method as Solved (Method (..))

data Method s scope = Method
  { position :: !Position,
    annotation :: !(Scheme s scope)
  }

check :: Context s scope -> Stage2.Method scope -> ST s (Method s scope)
check context Stage2.Method {position, annotation} = do
  annotation <- Scheme.check context annotation
  pure Method {position, annotation}

solve :: Context s scope -> Method s scope -> ST s (Solved.Method scope)
solve context Method {annotation} = do
  annotation <- Scheme.solve context annotation
  pure $ Solved.Method {annotation}
