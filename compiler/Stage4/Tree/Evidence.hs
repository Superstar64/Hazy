module Stage4.Tree.Evidence (Evidence (..)) where

import qualified Data.Vector.Strict as Strict
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Index.Evidence as Evidence
import qualified Stage4.Shift as Shift2

data Evidence scope
  = Variable
      { variable :: !(Evidence.Index scope)
      }
  | Call
      { function :: !(Evidence scope),
        arguments :: !(Strict.Vector (Evidence scope))
      }
  | Super
      { base :: !(Evidence scope),
        index :: !Int
      }
  deriving (Show)

instance Shift Evidence where
  shift = shiftDefault

instance Shift.Functor Evidence where
  map = Shift2.mapDefault

instance Shift2.Functor Evidence where
  map category = \case
    Variable {variable} ->
      Variable
        { variable = Shift2.map category variable
        }
    Call {function, arguments} ->
      Call
        { function = Shift2.map category function,
          arguments = Shift2.map category <$> arguments
        }
    Super {base, index} ->
      Super
        { base = Shift2.map category base,
          index
        }
