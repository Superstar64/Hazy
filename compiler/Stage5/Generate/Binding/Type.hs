module Stage5.Generate.Binding.Type where

import Data.Map (Map)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage5.Generate.Binding.Term as Term

data Binding scope = Binding
  { classInstances :: !(Map (Type2.Index scope) (Term.Binding scope)),
    dataInstances :: !(Map (Type2.Index scope) (Term.Binding scope))
  }

instance Shift Binding where
  shift = shiftDefault

instance Shift.Functor Binding where
  map category Binding {classInstances, dataInstances} =
    Binding
      { classInstances = Shift.mapmap category $ fmap (Shift.map category) classInstances,
        dataInstances = Shift.mapmap category $ fmap (Shift.map category) dataInstances
      }
