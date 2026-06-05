module Semantic.Check.Temporary.Method where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import Semantic.Check.Temporary.Scheme (Scheme)
import qualified Semantic.Check.Temporary.Scheme as Scheme
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Method as Semantic (Method (..))
import qualified Semantic.Tree.Method as Solved (Method (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.Variable (Variable)

data Method s scope = Method
  { position :: !Position,
    name :: !Variable,
    annotation :: !(Scheme s scope)
  }

check :: Context s scope -> Semantic.Method Resolve scope -> ST s (Method s scope)
check context Semantic.Method {position, name, annotation} = do
  annotation <- Scheme.check context annotation
  pure Method {position, name, annotation}

solve :: Context s scope -> Method s scope -> Unify.Solve s (Solved.Method Check scope)
solve context Method {position, name, annotation} = do
  annotation <- Scheme.solve context annotation
  pure $ Solved.Method {position, name, annotation}
