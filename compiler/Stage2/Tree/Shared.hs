{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Shared where

import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Pattern (Pattern)
import Stage2.Tree.RightHandSide (RightHandSide)

data Shared scope = Shared
  { patternx :: !(Pattern scope),
    definition :: !(RightHandSide scope)
  }
  deriving (Show)

instance Shift Shared where
  shift = shiftDefault

instance Shift.Functor Shared where
  map category Shared {patternx, definition} =
    Shared
      { patternx = Shift.map category patternx,
        definition = Shift.map category definition
      }
