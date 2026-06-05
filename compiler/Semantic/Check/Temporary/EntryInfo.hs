module Semantic.Check.Temporary.EntryInfo where

import qualified Semantic.Check.Simple.EntryInfo as Solved
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data EntryInfo s scope = EntryInfo
  { position :: !Position,
    strict :: !(Unify.Type s scope)
  }

solve :: EntryInfo s scope -> Unify.Solve s (Solved.EntryInfo scope)
solve EntryInfo {position, strict} = do
  strict <- Unify.solve position strict
  pure Solved.EntryInfo {strict}
