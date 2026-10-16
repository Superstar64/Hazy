module Semantic.Check.Temporary.EntryInfo where

import qualified Semantic.Check.Info.Entry as Solved
import qualified Semantic.Unify as Unify (Solve, Type, solve)
import Syntax.Position (Position)

data EntryInfo s scope = EntryInfo
  { position :: !Position,
    levity :: !(Unify.Type s scope)
  }

solve :: EntryInfo s scope -> Unify.Solve s (Solved.EntryInfo scope)
solve EntryInfo {position, levity} = do
  levity <- Unify.solve position levity
  pure Solved.EntryInfo {levity}
