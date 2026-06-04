module Stage2.Tree.PatternField where

import Stage2.Shift (Shift (shift), shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage2.Tree.Pattern (Pattern)
import {-# SOURCE #-} qualified Stage2.Tree.Pattern as Pattern (neverFails)

data Field stage scope = Field
  { index :: !Int,
    patternx :: !(Pattern stage scope)
  }
  deriving (Show)

instance Shift (Field stage) where
  shift = shiftDefault

instance Shift.Functor (Field stage) where
  map category = \case
    Field index patternx -> Field index (Shift.map category patternx)

neverFails :: Field stage scope -> Bool
neverFails Field {patternx} = Pattern.neverFails patternx
