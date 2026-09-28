module Semantic.Index.Evidence where

import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment ((:+)))
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data Index scope
  = Index !(Evidence0.Index scope)
  | Direct !(Type2.Index scope) !(Type2.Index scope)
  deriving (Eq, Show)

assumed :: Int -> Index (Scope.Local ':+ scopes)
assumed = Index . Evidence0.Assumed

instance Shift0.Functor Index where
  map = Shift.mapDefault

instance Shift.Functor Index where
  map category = \case
    Index index -> Index (Shift.map category index)
    Direct classx head -> Direct (Shift.map category classx) (Shift.map category head)

instance Shift.PartialUnshift Index where
  partialUnshift fail = \case
    Index index -> Index <$> Shift.partialUnshift fail index
    Direct classx head ->
      Direct
        <$> Shift.partialUnshift fail classx
        <*> Shift.partialUnshift fail head
