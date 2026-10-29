module Semantic.Check.Temporary.SelectorInfo where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Semantic.Tree.Info.Selector as Solved
import qualified Semantic.Unify as Unify (Solve, Type, solve)
import Syntax.Position (Position)

data SelectorInfo s scope
  = Newtype
  | Uniform
      { position :: !Position,
        levity :: !(Unify.Type s scope)
      }
  | Disjoint
      { position :: !Position,
        select :: !(Strict.Vector (Select s scope))
      }

data Select s scope
  = Select
      { entryCount :: !Int,
        specificLevity :: !(Unify.Type s scope),
        selection :: !Int
      }
  | Bottom
      { entryCount :: !Int
      }

solve :: SelectorInfo s scope -> Unify.Solve s (Solved.SelectorInfo scope)
solve = \case
  Newtype -> pure Solved.Newtype
  Uniform {position, levity} -> do
    levity <- Unify.solve position levity
    pure Solved.Uniform {levity}
  Disjoint {position, select} -> do
    select <- traverse (solveSelect position) select
    pure Solved.Disjoint {select}

solveSelect :: Position -> Select s scope -> Unify.Solve s (Solved.Select scope)
solveSelect position = \case
  Select {entryCount, specificLevity, selection} -> do
    specificLevity <- Unify.solve position specificLevity
    pure Solved.Select {entryCount, specificLevity, selection}
  Bottom {entryCount} -> pure Solved.Bottom {entryCount}
