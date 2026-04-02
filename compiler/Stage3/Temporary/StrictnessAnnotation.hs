module Stage3.Temporary.StrictnessAnnotation where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import qualified Stage2.Tree.StrictnessAnnotation as Stage2
import Stage3.Check.Context (Context)
import qualified Stage3.Synonym as Synonym
import Stage3.Temporary.Type (Type)
import qualified Stage3.Temporary.Type as Type
import qualified Stage3.Tree.StrictnessAnnotation as Solved
import qualified Stage3.Unify as Unify

data StrictnessAnnotation s scope
  = Lazy
  | Strict
  | Polymorphic
      { levity :: !(Type s scope)
      }

check :: Context s scope -> Stage2.StrictnessAnnotation Position scope -> ST s (StrictnessAnnotation s scope)
check context = \case
  Stage2.Lazy -> pure Lazy
  Stage2.Strict -> pure Strict
  Stage2.Polymorphic {levity} -> do
    levity <- Type.check context Unify.levity levity
    pure Polymorphic {levity}

solve :: Synonym.Context s scope -> StrictnessAnnotation s scope -> ST s (Solved.StrictnessAnnotation scope)
solve context = \case
  Lazy -> pure Solved.Lazy
  Strict -> pure Solved.Strict
  Polymorphic {levity} -> do
    levity <- Type.solve context levity
    pure Solved.Polymorphic {levity}
