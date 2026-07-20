{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Method where

import Semantic.FreeVariables (FreeTypeVariables (freeTypeVariables))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Scheme (Scheme)
import Syntax.Position (Position)
import Syntax.Variable (Variable)

data Method stage scope = Method
  { position :: !Position,
    name :: !Variable,
    annotation :: !(Scheme Position stage scope)
  }
  deriving (Show)

instance Shift0.Functor (Method stage) where
  map = Shift.mapDefault

instance Shift.Functor (Method stage) where
  map category Method {position, name, annotation} =
    Method
      { position,
        name,
        annotation = Shift.map category annotation
      }

instance FreeTypeVariables Method where
  freeTypeVariables target Method {annotation} = freeTypeVariables target annotation
