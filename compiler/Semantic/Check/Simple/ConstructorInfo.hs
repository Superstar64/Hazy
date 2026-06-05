module Semantic.Check.Simple.ConstructorInfo where

import qualified Core.Shift as Shift2
import qualified Data.Vector.Strict as Strict
import Semantic.Check.Simple.EntryInfo (EntryInfo)
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift (..), shiftDefault)
import qualified Semantic.Shift as Shift

data ConstructorInfo scope
  = ConstructorInfo
      { entries :: !(Strict.Vector (EntryInfo scope))
      }
  | Newtype
  deriving (Show)

instance Scope.Show ConstructorInfo where
  showsPrec = showsPrec

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
