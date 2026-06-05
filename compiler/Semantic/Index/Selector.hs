module Semantic.Index.Selector where

import qualified Semantic.Index.Type2 as Type2
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift

data Index scope = Index
  { typeIndex :: !(Type2.Index scope),
    selectorIndex :: !Int
  }
  deriving (Show, Eq, Ord)

instance Shift Index where
  shift = shiftDefault

instance Shift.Functor Index where
  map category (Index typeIndex selectorIndex) = Index (Shift.map category typeIndex) selectorIndex
