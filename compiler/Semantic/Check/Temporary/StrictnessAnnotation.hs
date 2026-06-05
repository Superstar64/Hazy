module Semantic.Check.Temporary.StrictnessAnnotation where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import Semantic.Check.Temporary.Type (Type)
import qualified Semantic.Check.Temporary.Type as Type
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.StrictnessAnnotation as Semantic
import qualified Semantic.Tree.StrictnessAnnotation as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data StrictnessAnnotation s scope
  = Lazy
  | Strict
  | Polymorphic
      { levity :: !(Type s scope)
      }

check :: Context s scope -> Semantic.StrictnessAnnotation Position Resolve scope -> ST s (StrictnessAnnotation s scope)
check context = \case
  Semantic.Lazy -> pure Lazy
  Semantic.Strict -> pure Strict
  Semantic.Polymorphic {levity} -> do
    levity <- Type.check context Unify.levity levity
    pure Polymorphic {levity}

solve ::
  Context s scope ->
  StrictnessAnnotation s scope ->
  Unify.Solve s (Solved.StrictnessAnnotation Position Check scope)
solve context = \case
  Lazy -> pure Solved.Lazy
  Strict -> pure Solved.Strict
  Polymorphic {levity} -> do
    levity <- Type.solve context levity
    pure Solved.Polymorphic {levity}
