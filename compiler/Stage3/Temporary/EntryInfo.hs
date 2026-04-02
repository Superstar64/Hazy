module Stage3.Temporary.EntryInfo where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import qualified Stage3.Tree.EntryInfo as Solved
import {-# SOURCE #-} qualified Stage3.Unify as Unify

data EntryInfo s scope = EntryInfo
  { position :: !Position,
    strict :: !(Unify.Type s scope)
  }

instance Unify.Zonk EntryInfo where
  zonk zonker EntryInfo {position, strict} = do
    strict <- Unify.zonk zonker strict
    pure EntryInfo {position, strict}

solve :: EntryInfo s scope -> ST s (Solved.EntryInfo scope)
solve EntryInfo {position, strict} = do
  strict <- Unify.solve position strict
  pure Solved.EntryInfo {strict}
