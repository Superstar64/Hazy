{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Method where

import Semantic.FreeVariables (FreeTypeVariables (freeTypeVariables))
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Tree.Scheme (Scheme)
import Syntax.Position (Position)
import Syntax.Variable (Variable)

data Method stage scope = Method
  { position :: !Position,
    name :: !Variable,
    annotation :: !(Scheme Position stage scope)
  }
  deriving (Show)

instance Shift (Method stage) where
  shift = shiftDefault

instance Shift.Functor (Method stage) where
  map category Method {position, name, annotation} =
    Method
      { position,
        name,
        annotation = Shift.map category annotation
      }

instance FreeTypeVariables Method where
  freeTypeVariables target Method {annotation} = freeTypeVariables target annotation
