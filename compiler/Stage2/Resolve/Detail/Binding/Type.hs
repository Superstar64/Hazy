module Stage2.Resolve.Detail.Binding.Type where

import Data.Map (Map)
import Data.Void (absurd)
import Stage1.Variable (Variable)
import qualified Stage2.Index.Type3 as Type3
import Stage2.Resolve.Functor.Same (Same (..))
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift

data Binding scope = Binding
  { index :: !(Type3.Index scope),
    methods :: !(Map Variable Int)
  }

instance Same (Binding scope) where
  same abort left right
    | left == right = left
    | otherwise = absurd abort

instance Eq (Binding scope) where
  left == right = index left == index right

instance Shift Binding where
  shift = shiftDefault

instance Shift.Functor Binding where
  map category Binding {index, methods} =
    Binding
      { index = Shift.map category index,
        methods
      }
