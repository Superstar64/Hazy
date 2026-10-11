module Core.Tree.Argument where

import qualified Core.Substitute as Substitute
import Core.Tree.EntryInfo (EntryInfo)
import {-# SOURCE #-} Core.Tree.Expression (Expression)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data Argument scope = Argument
  { argument :: !(Expression scope),
    info :: !(EntryInfo scope)
  }
  deriving (Show)

instance Shift0.Functor Argument where
  map = Shift.mapDefault

instance Shift.Functor Argument where
  map = Substitute.mapDefault

instance Substitute.Functor Argument where
  map category Argument {argument, info} =
    Argument
      { argument = Substitute.map category argument,
        info = Substitute.map category info
      }
