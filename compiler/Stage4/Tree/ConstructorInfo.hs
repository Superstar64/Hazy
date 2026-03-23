module Stage4.Tree.ConstructorInfo where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage4.Tree.EntryInfo (EntryInfo)

newtype ConstructorInfo = ConstructorInfo
  { entries :: Strict.Vector EntryInfo
  }
  deriving (Show)
