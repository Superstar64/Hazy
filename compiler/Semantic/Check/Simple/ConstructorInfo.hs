module Semantic.Check.Simple.ConstructorInfo where

import qualified Data.Vector.Strict as Strict
import Semantic.Check.Simple.EntryInfo (EntryInfo)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

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

instance Shift0.Functor ConstructorInfo where
  map = Shift.mapDefault

instance Shift.Functor ConstructorInfo where
  map category = \case
    ConstructorInfo {entries} ->
      ConstructorInfo
        { entries = Shift.map category <$> entries
        }
    Newtype -> Newtype
