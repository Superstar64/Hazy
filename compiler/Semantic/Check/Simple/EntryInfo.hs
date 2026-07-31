module Semantic.Check.Simple.EntryInfo where

import qualified Core.Substitute as Substitute
import qualified Core.Tree.Type as Simple
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

newtype EntryInfo scope = EntryInfo
  { strict :: Simple.Type scope
  }
  deriving (Show)

instance Shift0.Functor EntryInfo where
  map = Shift.mapDefault

instance Shift.Functor EntryInfo where
  map = Substitute.mapDefault

instance Substitute.Functor EntryInfo where
  map category EntryInfo {strict} =
    EntryInfo
      { strict = Substitute.map category strict
      }
