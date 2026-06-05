module Semantic.Check.Temporary.ConstructorInfo where

import qualified Data.Vector.Strict as Strict
import qualified Semantic.Check.Simple.ConstructorInfo as Solved
import Semantic.Check.Temporary.EntryInfo (EntryInfo)
import qualified Semantic.Check.Temporary.EntryInfo as EntryInfo
import {-# SOURCE #-} qualified Semantic.Unify as Unify

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
