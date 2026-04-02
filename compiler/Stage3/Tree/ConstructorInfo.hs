module Stage3.Tree.ConstructorInfo where

import qualified Data.Vector.Strict as Strict
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage3.Tree.EntryInfo (EntryInfo)
import qualified Stage4.Shift as Shift2

data ConstructorInfo scope
  = ConstructorInfo
      { entries :: !(Strict.Vector (EntryInfo scope))
      }
  | Newtype
  deriving (Show)

entryCount :: ConstructorInfo scope -> Int
entryCount ConstructorInfo {entries} = length entries
entryCount Newtype = 1

instance Shift ConstructorInfo where
  shift = shiftDefault

instance Shift.Functor ConstructorInfo where
  map = Shift2.mapDefault

instance Shift2.Functor ConstructorInfo where
  map category = \case
    ConstructorInfo {entries} ->
      ConstructorInfo
        { entries = Shift2.map category <$> entries
        }
    Newtype -> Newtype
