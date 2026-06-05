{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Field where

import Semantic.FreeVariables (FreeTypeVariables (freeTypeVariables))
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Tree.Entry (Entry)
import Syntax.Position (Position)
import Syntax.Variable (Variable)

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

instance FreeTypeVariables Field where
  freeTypeVariables target Field {entry} = freeTypeVariables target entry
