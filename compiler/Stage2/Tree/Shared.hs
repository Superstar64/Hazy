{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Shared where

import Stage1.Position (Position)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Pattern (Pattern)
import Stage2.Tree.RightHandSide (RightHandSide)

data Shared scope = Shared
  { equalPosition :: !Position,
    patternx :: !(Pattern scope),
    definition :: !(RightHandSide scope)
  }
  deriving (Show)

instance Shift Shared where
  shift = shiftDefault

instance Shift.Functor Shared where
  map category Shared {equalPosition, patternx, definition} =
    Shared
      { equalPosition,
        patternx = Shift.map category patternx,
        definition = Shift.map category definition
      }
