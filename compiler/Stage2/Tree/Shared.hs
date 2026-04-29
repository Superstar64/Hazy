{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Shared where

import Stage1.Position (Position)
import Stage2.FreeVariables (FreeTermVariables (..))
import Stage2.Layout (Normal)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition2 (Definition2, Inferred, Share)
import Stage2.Tree.Pattern (Pattern)

data Shared locality scope = Shared
  { equalPosition :: !Position,
    patternx :: !(Pattern scope),
    definition :: !(Definition2 locality Share Inferred Normal scope)
  }
  deriving (Show)

instance Shift (Shared locality) where
  shift = shiftDefault

instance Shift.Functor (Shared locality) where
  map category Shared {equalPosition, patternx, definition} =
    Shared
      { equalPosition,
        patternx = Shift.map category patternx,
        definition = Shift.map category definition
      }

instance FreeTermVariables (Shared locality) where
  freeTermVariables target Shared {definition} = freeTermVariables target definition
