{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.GADTConstructor where

import Stage1.Position (Position)
import Stage1.Variable (Constructor)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Scheme (Scheme)

data GADTConstructor scope = GADTConstructor
  { position :: !Position,
    name :: !Constructor,
    typex :: !(Scheme Position scope)
  }
  deriving (Show)

instance Shift GADTConstructor where
  shift = shiftDefault

instance Shift.Functor GADTConstructor where
  map category GADTConstructor {position, name, typex} =
    GADTConstructor
      { position,
        name,
        typex = Shift.map category typex
      }
