module Stage3.Temporary.Method where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import qualified Stage2.Tree.Method as Stage2 (Method (..))
import Stage3.Check.Context (Context)
import qualified Stage3.Synonym as Synonym
import Stage3.Temporary.Scheme (Scheme)
import qualified Stage3.Temporary.Scheme as Scheme
import qualified Stage3.Tree.Method as Solved (Method (..))
import qualified Stage4.Tree.Scheme as Simple (simplify)

data Method s scope = Method
  { position :: !Position,
    annotation :: !(Scheme s scope)
  }

check :: Context s scope -> Stage2.Method scope -> ST s (Method s scope)
check context Stage2.Method {position, annotation} = do
  annotation <- Scheme.check context annotation
  pure Method {position, annotation}

solve :: Synonym.Context s scope -> Method s scope -> ST s (Solved.Method scope)
solve context Method {annotation} = do
  annotation <- Scheme.solve context annotation
  let annotation' = Simple.simplify annotation
  pure $ Solved.Method {annotation, annotation'}
