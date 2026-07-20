module Semantic.Index.Type3 where

import Data.Functor.Identity (Identity (Identity, runIdentity))
import qualified Semantic.Index.Type as Type1
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Prelude hiding (map, traverse)

data Index scope
  = Index !(Type2.Index scope)
  | Type
  | Constraint
  | Small
  | Large
  | Universe
  | Levity
  deriving (Show, Eq, Ord)

map :: (Type1.Index scope -> Type1.Index scope') -> Index scope -> Index scope'
map run = runIdentity . traverse (Identity . run)

traverse :: (Applicative m) => (Type1.Index scope -> m (Type1.Index scope')) -> Index scope -> m (Index scope')
traverse run = \case
  Index index -> Index <$> Type2.traverse run index
  Type -> pure Type
  Constraint -> pure Constraint
  Small -> pure Small
  Large -> pure Large
  Universe -> pure Universe
  Levity -> pure Levity

instance Shift0.Functor Index where
  map = Shift.mapDefault

instance Shift.Functor Index where
  map category = map (Shift.map category)

instance Shift.PartialUnshift Index where
  partialUnshift abort = traverse (Shift.partialUnshift abort)

toType2 :: Type2.Index scope -> Index scope -> Type2.Index scope
toType2 _ (Index index) = index
toType2 index _ = index
