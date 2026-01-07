module Stage3.Index.Evidence0 where

import Data.Kind (Type)
import Data.Void (absurd, vacuous)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift

type Index :: Environment -> Type
data Index scope where
  Assumed :: !Int -> Index (Scope.Local ':+ scopes)
  Shift :: !(Index scopes) -> Index (scope ':+ scopes)

instance Eq (Index scope) where
  Assumed left == Assumed right = left == right
  Shift left == Shift right = left == right
  _ == _ = False

instance Show (Index scopes) where
  showsPrec d (Assumed i) =
    showParen (d > 10) $
      showString "Assumed "
        . showsPrec 11 i
  showsPrec d (Shift ix) =
    showParen (d > 10) $
      showString "Shift "
        . showsPrec 11 ix

instance Shift Index where
  shift = shiftDefault

instance Shift.Functor Index where
  map Shift.Id index = index
  map (Shift.Over _) (Assumed index) = Assumed index
  map (Shift.Over category) (Shift index) = Shift (Shift.map category index)
  map Shift.Shift index = Shift index
  map (category1 Shift.:. category2) index = Shift.map category1 $ Shift.map category2 index
  map Shift.Rotate index = case index of
    Assumed index -> Shift (Assumed index)
    Shift (Assumed index) -> Assumed index
    Shift (Shift index) -> Shift (Shift index)
  map (Shift.Unshift _) (Shift index) = index
  map (Shift.Unshift abort) Assumed {} = absurd abort

instance Shift.PartialUnshift Index where
  partialUnshift abort (Assumed _) = vacuous abort
  partialUnshift _ (Shift index) = pure index
