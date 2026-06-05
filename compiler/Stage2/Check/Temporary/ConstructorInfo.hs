module Stage2.Check.Temporary.ConstructorInfo where

import qualified Data.Vector.Strict as Strict
import qualified Stage2.Check.Simple.ConstructorInfo as Solved
import Stage2.Check.Temporary.EntryInfo (EntryInfo)
import qualified Stage2.Check.Temporary.EntryInfo as EntryInfo
import {-# SOURCE #-} qualified Stage2.Unify as Unify

data ConstructorInfo s scope
  = ConstructorInfo
      { entries :: !(Strict.Vector (EntryInfo s scope))
      }
  | Newtype

solve :: ConstructorInfo s scope -> Unify.Solve s (Solved.ConstructorInfo scope)
solve = \case
  ConstructorInfo {entries} -> do
    entries <- traverse EntryInfo.solve entries
    pure Solved.ConstructorInfo {entries}
  Newtype -> pure Solved.Newtype
