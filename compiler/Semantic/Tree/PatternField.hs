module Semantic.Tree.PatternField where

import Semantic.Shift (Shift (shift), shiftDefault)
import qualified Semantic.Shift as Shift
import {-# SOURCE #-} Semantic.Tree.Pattern (Pattern)
import {-# SOURCE #-} qualified Semantic.Tree.Pattern as Pattern (neverFails)

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
