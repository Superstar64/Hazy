module Stage3.Temporary.Entry where

import Control.Monad.ST (ST)
import Error (Position, unsupportedFeaturePolymorphicComponents)
import qualified Stage2.Shift as Shift
import qualified Stage2.Tree.Entry as Stage2 (Entry (..))
import qualified Stage2.Tree.Scheme as Stage2 (Scheme (Scheme, result))
import Stage3.Check.Context (Context (..))
import qualified Stage3.Synonym as Synonym
import Stage3.Temporary.Type (Type)
import qualified Stage3.Temporary.Type as Type
import qualified Stage3.Tree.Entry as Solved
import qualified Stage3.Unify as Unify

data Entry s scope = Entry
  { entry :: !(Type s scope),
    strict :: !Bool
  }

check :: Context s scope -> Stage2.Entry Position scope -> ST s (Entry s scope)
check context Stage2.Entry {startPosition, entry = Stage2.Scheme {result}, strict} = do
  entry <- pure $ Shift.map (Shift.Unshift $ unsupportedFeaturePolymorphicComponents startPosition) result
  entry <- Type.check context Unify.typex entry
  pure
    Entry
      { entry,
        strict
      }

solve :: Synonym.Context s scope -> Entry s scope -> ST s (Solved.Entry scope)
solve context Entry {entry, strict} = do
  entry <- Type.solve context entry
  pure
    Solved.Entry
      { entry,
        strict
      }
