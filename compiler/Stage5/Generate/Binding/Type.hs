module Stage5.Generate.Binding.Type where

import Data.Map (Map)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage5.Generate.Variable (Variable)

data Binding scope = Binding
  { classInstances :: !(Map (Type2.Index scope) Variable),
    dataInstances :: !(Map (Type2.Index scope) Variable)
  }

instance Shift Binding where
  shift = shiftDefault

instance Shift.Functor Binding where
  map category Binding {classInstances, dataInstances} =
    Binding
      { classInstances = Shift.mapInstances category classInstances,
        dataInstances = Shift.mapInstances category dataInstances
      }
