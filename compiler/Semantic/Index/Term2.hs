module Semantic.Index.Term2 where

import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Term as Term
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data Index scope
  = Index !(Term.Index scope)
  | Select !(Selector.Index scope)
  | Method !(Method.Index scope)
  | RunST
  deriving (Eq, Show)

instance Shift0.Functor Index where
  map = Shift.mapDefault

instance Shift.Functor Index where
  map category = \case
    Index index -> Index (Shift.map category index)
    Select selector -> Select (Shift.map category selector)
    Method method -> Method (Shift.map category method)
    RunST -> RunST

index = Index

runST = RunST
