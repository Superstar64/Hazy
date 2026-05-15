module Stage2.Tree.Inferred where

import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check, Resolve)
import {-# SOURCE #-} qualified Stage4.Tree.Type as Simple

data Inferred stage scope where
  Infer :: Inferred Resolve scope
  Solved :: !(Simple.Type scope) -> Inferred Check scope

instance Show (Inferred stage scope) where
  showsPrec d = \case
    Infer -> showString "Infer"
    Solved typex -> showParen (d > 10) $ showString "Solved " . showsPrec 11 typex

instance Shift (Inferred stage) where
  shift = shiftDefault

instance Shift.Functor (Inferred stage) where
  map category = \case
    Infer -> Infer
    Solved typex -> Solved (Shift.map category typex)

get :: Inferred Check scope -> Simple.Type scope
get (Solved typex) = typex
