module Semantic.Check.Temporary.EntryInfo where

import qualified Semantic.Check.Info.Entry as Solved
import qualified Semantic.Unify as Unify (Solve, Type, solve)
import Syntax.Position (Position)

data EntryInfo s scope = EntryInfo
  { position :: !Position,
    strict :: !(Unify.Type s scope)
  }

solve :: EntryInfo s scope -> Unify.Solve s (Solved.EntryInfo scope)
solve EntryInfo {position, strict} = do
  strict <- Unify.solve position strict
  pure Solved.EntryInfo {strict}
