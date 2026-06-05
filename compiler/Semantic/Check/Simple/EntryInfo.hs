module Semantic.Check.Simple.EntryInfo where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import qualified Core.Tree.Type as Simple
import Semantic.Shift (Shift (..), shiftDefault)
import qualified Semantic.Shift as Shift

newtype EntryInfo scope = EntryInfo
  { strict :: Simple.Type scope
  }
  deriving (Show)

instance Shift EntryInfo where
  shift = shiftDefault

instance Shift.Functor EntryInfo where
  map = Shift2.mapDefault

instance Shift2.Functor EntryInfo where
  map = Substitute.mapDefault

instance Substitute.Functor EntryInfo where
  map category EntryInfo {strict} =
    EntryInfo
      { strict = Substitute.map category strict
      }
