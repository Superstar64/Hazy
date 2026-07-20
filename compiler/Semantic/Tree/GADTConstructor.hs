{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.GADTConstructor where

import Semantic.FreeVariables (FreeTypeVariables (freeTypeVariables))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Scheme (Scheme)
import Syntax.Position (Position)
import Syntax.Variable (Constructor)

data GADTConstructor stage scope = GADTConstructor
  { position :: !Position,
    name :: !Constructor,
    typex :: !(Scheme Position stage scope)
  }
  deriving (Show)

instance Shift0.Functor (GADTConstructor stage) where
  map = Shift.mapDefault

instance Shift.Functor (GADTConstructor stage) where
  map category GADTConstructor {position, name, typex} =
    GADTConstructor
      { position,
        name,
        typex = Shift.map category typex
      }

instance FreeTypeVariables GADTConstructor where
  freeTypeVariables target GADTConstructor {typex} = freeTypeVariables target typex
