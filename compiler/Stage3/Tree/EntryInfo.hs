module Stage3.Tree.EntryInfo where

import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import qualified Stage4.Tree.Type as Simple

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
