module Semantic.Index.Type3 where

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

instance Shift0.Functor Index where
  map = Shift.mapDefault

instance Shift.Functor Index where
  map category = \case
    Index index -> Index $ Shift.map category index
    Type -> Type
    Constraint -> Constraint
    Small -> Small
    Large -> Large
    Universe -> Universe
    Levity -> Levity

instance Shift.PartialUnshift Index where
  partialUnshift abort = \case
    Index index -> Index <$> Shift.partialUnshift abort index
    Type -> pure Type
    Constraint -> pure Constraint
    Small -> pure Small
    Large -> pure Large
    Universe -> pure Universe
    Levity -> pure Levity

toType2 :: Type2.Index scope -> Index scope -> Type2.Index scope
toType2 _ (Index index) = index
toType2 index _ = index
