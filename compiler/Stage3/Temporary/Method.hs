module Stage3.Temporary.Method where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict
import Stage1.Position (Position)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (shift)
import qualified Stage2.Tree.Definition as Stage2 (Definition (..))
import qualified Stage2.Tree.Method as Stage2 (Method (..))
import Stage3.Check.Context (Context)
import qualified Stage3.Simple.Scheme as Simple (Scheme (..), augment, simplify)
import qualified Stage3.Simple.SchemeOver as Simple (SchemeOver (..))
import qualified Stage3.Simple.Type as Simple.Type
import qualified Stage3.Synonym as Synonym
import {-# SOURCE #-} Stage3.Temporary.Definition (Definition)
import {-# SOURCE #-} qualified Stage3.Temporary.Definition as Definition
import Stage3.Temporary.Scheme (Scheme)
import qualified Stage3.Temporary.Scheme as Scheme
import qualified Stage3.Tree.Method as Solved (Method (..))
import qualified Stage3.Tree.Scheme as Solved (Scheme (..))

data Method s scope = Method
  { position :: !Position,
    annotation :: !(Scheme s scope),
    definition :: !(Strict.Maybe (Stage2.Definition scope))
  }

check1 :: Context s scope -> Stage2.Method scope -> ST s (Method s scope)
check1 context Stage2.Method {position, annotation, definition} = do
  annotation <- Scheme.check context annotation
  pure Method {position, annotation, definition}

solve1 :: Synonym.Context s scope -> Method s scope -> ST s (Method2 scope)
solve1 context Method {position = position2, annotation, definition = definition2} = do
  annotation2 <- Scheme.solve context annotation
  let annotation2' = Simple.simplify annotation2
  pure $ Method2 {position2, annotation2, annotation2', definition2}

data Method2 scope = Method2
  { position2 :: !Position,
    annotation2 :: !(Solved.Scheme scope),
    annotation2' :: !(Simple.Scheme scope),
    definition2 :: !(Strict.Maybe (Stage2.Definition scope))
  }

check2 :: Context s scope -> Method2 scope -> ST s (Method3 s scope)
check2 context Method2 {position2, annotation2 = annotation3, annotation2' = annotation3', definition2}
  | Simple.Scheme Simple.SchemeOver {parameters, constraints, result} <- annotation3' = do
      context <- Simple.augment position2 parameters constraints context
      definition3 <- traverse (Definition.check context (Simple.Type.lift result) . shift) definition2
      pure $ Method3 {annotation3, annotation3', definition3}

data Method3 s scope = Method3
  { annotation3 :: !(Solved.Scheme scope),
    annotation3' :: !(Simple.Scheme scope),
    definition3 :: !(Strict.Maybe (Definition s (Local ':+ scope)))
  }

solve2 :: Method3 s scope -> ST s (Solved.Method scope)
solve2 Method3 {annotation3 = annotation, annotation3' = annotation', definition3} = do
  definition <- traverse Definition.solve definition3
  pure $ Solved.Method {annotation, annotation', definition}
