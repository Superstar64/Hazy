module Core.Tree.ConstructorInfo where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.EntryInfo (EntryInfo)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

newtype ConstructorInfo scope = ConstructorInfo
  { entries :: Strict.Vector (EntryInfo scope)
  }
  deriving (Show)

instance Shift0.Functor ConstructorInfo where
  map = Shift.mapDefault

instance Shift.Functor ConstructorInfo where
  map = Shift2.mapDefault

instance Shift2.Functor ConstructorInfo where
  map = Substitute.mapDefault

instance Substitute.Functor ConstructorInfo where
  map category ConstructorInfo {entries} =
    ConstructorInfo
      { entries = Substitute.map category <$> entries
      }
