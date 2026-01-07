module Stage2.Index.Term2 where

import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Index.Term as Term
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift

data Index scope
  = Index !(Term.Index scope)
  | Select !(Selector.Index scope)
  | Method !(Method.Index scope)
  | RunST
  deriving (Eq, Show)

instance Shift Index where
  shift = shiftDefault

instance Shift.Functor Index where
  map category = \case
    Index index -> Index (Shift.map category index)
    Select selector -> Select (Shift.map category selector)
    Method method -> Method (Shift.map category method)
    RunST -> RunST

index = Index

runST = RunST
