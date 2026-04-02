module Stage4.Tree.ConstructorInfo where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.EntryInfo (EntryInfo)

newtype ConstructorInfo scope = ConstructorInfo
  { entries :: Strict.Vector (EntryInfo scope)
  }
  deriving (Show)

instance Shift ConstructorInfo where
  shift = shiftDefault

instance Shift.Functor ConstructorInfo where
  map = Shift2.mapDefault

instance Shift2.Functor ConstructorInfo where
  map = Substitute.mapDefault

instance Substitute.Functor ConstructorInfo where
  map category ConstructorInfo {entries} =
    ConstructorInfo
      { entries = Substitute.map category <$> entries
      }
