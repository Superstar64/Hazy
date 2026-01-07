module Stage3.Index.Evidence where

import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Index.Evidence0 as Evidence0

data Index scope
  = Index !(Evidence0.Index scope)
  | Direct !(Type2.Index scope) !(Type2.Index scope)
  deriving (Eq, Show)

assumed :: Int -> Index (Scope.Local ':+ scopes)
assumed = Index . Evidence0.Assumed

instance Shift Index where
  shift = shiftDefault

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
