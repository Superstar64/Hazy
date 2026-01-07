module Stage2.Index.Type3 where

import Data.Functor.Identity (Identity (Identity, runIdentity))
import qualified Stage2.Index.Type as Type1
import qualified Stage2.Index.Type2 as Type2
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Prelude hiding (map, traverse)

data Index scope
  = Index !(Type2.Index scope)
  | Type
  | Constraint
  | Small
  | Large
  | Universe
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

instance Shift Index where
  shift = shiftDefault

instance Shift.Functor Index where
  map category = map (Shift.map category)

instance Shift.PartialUnshift Index where
  partialUnshift abort = traverse (Shift.partialUnshift abort)

toType2 :: Type2.Index scope -> Index scope -> Type2.Index scope
toType2 _ (Index index) = index
toType2 index _ = index
