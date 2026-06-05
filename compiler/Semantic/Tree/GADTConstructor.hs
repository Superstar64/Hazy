{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.GADTConstructor where

import Semantic.FreeVariables (FreeTypeVariables (freeTypeVariables))
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Tree.Scheme (Scheme)
import Syntax.Position (Position)
import Syntax.Variable (Constructor)

data GADTConstructor stage scope = GADTConstructor
  { position :: !Position,
    name :: !Constructor,
    typex :: !(Scheme Position stage scope)
  }
  deriving (Show)

instance Shift (GADTConstructor stage) where
  shift = shiftDefault

instance Shift.Functor (GADTConstructor stage) where
  map category GADTConstructor {position, name, typex} =
    GADTConstructor
      { position,
        name,
        typex = Shift.map category typex
      }

instance FreeTypeVariables GADTConstructor where
  freeTypeVariables target GADTConstructor {typex} = freeTypeVariables target typex
