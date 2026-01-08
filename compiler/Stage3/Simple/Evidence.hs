module Stage3.Simple.Evidence where

import qualified Data.Vector.Strict as Strict
import qualified Stage2.Index.Term as Term
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Index.Evidence as Evidence
import {-# SOURCE #-} qualified Stage3.Unify as Unify

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

instance Term.Functor Evidence where
  map Term.Category {general} = Shift.map general

lift :: Evidence scope -> Unify.Evidence s scope
lift = \case
  Proof {proof, arguments} -> Unify.proof proof (fmap lift arguments)
  Super {base, index} -> Unify.super (lift base) index
