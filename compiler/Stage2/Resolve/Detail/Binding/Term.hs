module Stage2.Resolve.Detail.Binding.Term where

import Data.Void (absurd)
import Stage1.Tree.Fixity (Fixity)
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Index.Term2 as Term2
import Stage2.Resolve.Functor.Same (Same (..))
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift

data Binding scope = Binding
  { index :: !(Term2.Index scope),
    fixity :: !Fixity,
    selector :: Selector scope
  }

instance Same (Binding scope) where
  same abort left right
    | left == right = left
    | otherwise = absurd abort

instance Eq (Binding scope) where
  left == right = index left == index right

data Selector scope
  = Selector !(Selector.Index scope)
  | Normal
  deriving (Show)

instance Shift Selector where
  shift = shiftDefault

instance Shift.Functor Selector where
  map category = \case
    Selector index -> Selector (Shift.map category index)
    Normal -> Normal

instance Semigroup (Selector scope) where
  field@Selector {} <> _ = field
  Normal <> field = field

instance Monoid (Selector scope) where
  mempty = Normal
