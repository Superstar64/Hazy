module Stage2.Check.Temporary.Method where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage1.Variable (Variable)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Method as Solved (Method (..))
import qualified Stage2.Tree.Method as Stage2 (Method (..))
import Stage2.Check.Context (Context)
import Stage2.Check.Temporary.Scheme (Scheme)
import qualified Stage2.Check.Temporary.Scheme as Scheme
import qualified Stage2.Unify as Unify

data Method s scope = Method
  { position :: !Position,
    name :: !Variable,
    annotation :: !(Scheme s scope)
  }

check :: Context s scope -> Stage2.Method Resolve scope -> ST s (Method s scope)
check context Stage2.Method {position, name, annotation} = do
  annotation <- Scheme.check context annotation
  pure Method {position, name, annotation}

solve :: Context s scope -> Method s scope -> Unify.Solve s (Solved.Method Check scope)
solve context Method {position, name, annotation} = do
  annotation <- Scheme.solve context annotation
  pure $ Solved.Method {position, name, annotation}
