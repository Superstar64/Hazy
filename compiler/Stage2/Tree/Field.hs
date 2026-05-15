{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Field where

import Stage1.Position (Position)
import Stage1.Variable (Variable)
import Stage2.FreeVariables (FreeTypeVariables (freeTypeVariables))
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Entry (Entry)

data Field stage scope = Field
  { position :: !Position,
    name :: !Variable,
    entry :: !(Entry Position stage scope)
  }
  deriving (Show)

instance Shift (Field stage) where
  shift = shiftDefault

instance Shift.Functor (Field stage) where
  map category Field {position, name, entry} =
    Field
      { position,
        name,
        entry = Shift.map category entry
      }

instance FreeTypeVariables (Field stage) where
  freeTypeVariables target Field {entry} = freeTypeVariables target entry
