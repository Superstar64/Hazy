module Stage2.Index.Local where

import Data.Kind (Type)
import Data.Void (absurd, vacuous)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift

type Index :: Environment -> Type
data Index scopes where
  Local :: !Int -> Index (Local ':+ scopes)
  Shift :: !(Index scopes) -> Index (scope ':+ scopes)

instance Eq (Index scope) where
  Local local1 == Local local2 = local1 == local2
  Shift index1 == Shift index2 = index1 == index2
  _ == _ = False

instance Show (Index scope) where
  showsPrec d = \case
    Local local -> showParen (d > 10) $ showString "Local " . showsPrec 11 local
    Shift index -> showParen (d > 10) $ showString "Shift " . showsPrec 11 index

instance Shift Index where
  shift = shiftDefault

instance Shift.Functor Index where
  map Shift.Id index = index
  map Shift.Shift index = Shift index
  map (Shift.Over category) (Shift index) = Shift $ Shift.map category index
  map (Shift.Over _) (Local index) = Local index
  map (after Shift.:. before) index = Shift.map after (Shift.map before index)
  map Shift.Rotate index = case index of
    Local local -> Shift (Local local)
    Shift (Local local) -> Local local
    Shift (Shift index) -> Shift (Shift index)
  map (Shift.Unshift _) (Shift index) = index
  map (Shift.Unshift abort) Local {} = absurd abort

instance Shift.PartialUnshift Index where
  partialUnshift _ (Shift index) = pure index
  partialUnshift abort Local {} = vacuous abort
