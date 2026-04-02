module Stage3.Temporary.ConstructorInfo where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import Stage3.Temporary.EntryInfo (EntryInfo)
import qualified Stage3.Temporary.EntryInfo as EntryInfo
import qualified Stage3.Tree.ConstructorInfo as Solved
import {-# SOURCE #-} qualified Stage3.Unify as Unify

data ConstructorInfo s scope
  = ConstructorInfo
      { entries :: !(Strict.Vector (EntryInfo s scope))
      }
  | Newtype

instance Unify.Zonk ConstructorInfo where
  zonk zonker = \case
    ConstructorInfo {entries} -> do
      entries <- traverse (Unify.zonk zonker) entries
      pure ConstructorInfo {entries}
    Newtype -> pure Newtype

solve :: ConstructorInfo s scope -> ST s (Solved.ConstructorInfo scope)
solve = \case
  ConstructorInfo {entries} -> do
    entries <- traverse EntryInfo.solve entries
    pure Solved.ConstructorInfo {entries}
  Newtype -> pure Solved.Newtype
