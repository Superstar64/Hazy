module Stage3.Tree.ConstructorInfo where

import qualified Data.Vector.Strict as Strict
import Stage3.Tree.EntryInfo (EntryInfo)

data ConstructorInfo
  = ConstructorInfo
      { entries :: !(Strict.Vector EntryInfo)
      }
  | Newtype
  deriving (Show)

entryCount :: ConstructorInfo -> Int
entryCount ConstructorInfo {entries} = length entries
entryCount Newtype = 1
