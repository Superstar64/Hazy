module Generate.Binding.Type where

import Data.Map (Map)
import Generate.Variable (Variable)
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data Binding scope = Binding
  { classInstances :: !(Map (Type2.Index scope) Variable),
    dataInstances :: !(Map (Type2.Index scope) Variable)
  }

instance Shift0.Functor Binding where
  map = Shift.mapDefault

instance Shift.Functor Binding where
  map category Binding {classInstances, dataInstances} =
    Binding
      { classInstances = Shift.mapInstances category classInstances,
        dataInstances = Shift.mapInstances category dataInstances
      }
