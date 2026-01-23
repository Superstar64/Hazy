module Stage4.Tree.Evidence (Evidence (..)) where

import qualified Data.Vector.Strict as Strict
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Index.Evidence as Evidence
import qualified Stage4.Shift as Shift2

data Evidence scope
  = Proof
      { proof :: !(Evidence.Index scope),
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
  map category = \case
    Proof {proof, arguments} ->
      Proof
        { proof = Shift.map category proof,
          arguments = fmap (Shift.map category) arguments
        }
    Super {base, index} ->
      Super
        { base = Shift.map category base,
          index
        }

instance Shift2.Functor Evidence where
  map category = Shift.map (Shift2.general category)
