module Semantic.Check.Temporary.UpdateInfo where

import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Semantic.Tree.Info.Update as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data UpdateInfo s scope
  = Newtype
  | Updates
      { position :: !Position,
        updates :: !(Strict.Vector (Update s scope))
      }

data Update s scope = Update
  { levities :: !(Strict.Vector (Unify.Type s scope)),
    selectorIndexes :: !(Strict.Vector (Strict.Maybe Int))
  }

solve :: UpdateInfo s scope -> Unify.Solve s (Solved.UpdateInfo scope)
solve = \case
  Newtype -> pure Solved.Newtype
  Updates {position, updates} -> do
    updates <- traverse (solveUpdate position) updates
    pure Solved.Updates {updates}

solveUpdate :: Position -> Update s scope -> Unify.Solve s (Solved.Update scope)
solveUpdate position Update {levities, selectorIndexes} = do
  levities <- traverse (Unify.solve position) levities
  pure Solved.Update {levities, selectorIndexes}
