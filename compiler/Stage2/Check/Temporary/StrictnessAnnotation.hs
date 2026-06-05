module Stage2.Check.Temporary.StrictnessAnnotation where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.StrictnessAnnotation as Solved
import qualified Stage2.Tree.StrictnessAnnotation as Stage2
import Stage2.Check.Context (Context)
import Stage2.Check.Temporary.Type (Type)
import qualified Stage2.Check.Temporary.Type as Type
import qualified Stage2.Unify as Unify

data StrictnessAnnotation s scope
  = Lazy
  | Strict
  | Polymorphic
      { levity :: !(Type s scope)
      }

check :: Context s scope -> Stage2.StrictnessAnnotation Position Resolve scope -> ST s (StrictnessAnnotation s scope)
check context = \case
  Stage2.Lazy -> pure Lazy
  Stage2.Strict -> pure Strict
  Stage2.Polymorphic {levity} -> do
    levity <- Type.check context Unify.levity levity
    pure Polymorphic {levity}

solve :: Context s scope -> StrictnessAnnotation s scope ->
   Unify.Solve s (Solved.StrictnessAnnotation Position Check scope)
solve context = \case
  Lazy -> pure Solved.Lazy
  Strict -> pure Solved.Strict
  Polymorphic {levity} -> do
    levity <- Type.solve context levity
    pure Solved.Polymorphic {levity}
