module Semantic.Check.Temporary.Entry where

import Control.Monad.ST (ST)
import Error (Position, unsupportedFeaturePolymorphicComponents)
import Semantic.Check.Context (Context (..))
import Semantic.Check.Temporary.StrictnessAnnotation (StrictnessAnnotation)
import qualified Semantic.Check.Temporary.StrictnessAnnotation as StrictnessAnnotation
import Semantic.Check.Temporary.Type (Type)
import qualified Semantic.Check.Temporary.Type as Type
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Entry as Semantic (Entry (..), Restricted (..))
import qualified Semantic.Tree.Entry as Solved
import qualified Semantic.Tree.Scheme as Semantic (Scheme (Scheme, result))
import qualified Semantic.Unify as Unify

data Entry s scope = Entry
  { startPosition :: !Position,
    entry :: !(Type s scope),
    strict :: !(StrictnessAnnotation s scope)
  }

check :: Context s scope -> Semantic.Entry Position Resolve scope -> ST s (Entry s scope)
check context Semantic.Entry {startPosition, entry = Semantic.Canonical Semantic.Scheme {result}, strict} = do
  entry <- pure $ Shift.map (Shift.Unshift $ unsupportedFeaturePolymorphicComponents startPosition) result
  entry <- Type.check context Unify.typex entry
  strict <- StrictnessAnnotation.check context strict
  pure
    Entry
      { startPosition,
        entry,
        strict
      }

solve :: Context s scope -> Entry s scope -> Unify.Solve s (Solved.Entry Position Check scope)
solve context Entry {startPosition, entry, strict} = do
  entry <- Type.solve context entry
  strict <- StrictnessAnnotation.solve context strict
  pure
    Solved.Entry
      { startPosition,
        entry = Semantic.Restricted entry,
        strict
      }
