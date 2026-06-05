module Stage2.Check.Temporary.SelectorInfo where

import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import qualified Stage2.Check.Simple.SelectorInfo as Solved
import Stage2.Check.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Stage2.Check.Temporary.ConstructorInfo as ConstructorInfo
import {-# SOURCE #-} qualified Stage2.Unify as Unify

data SelectorInfo s scope
  = Uniform
      { position :: !Position,
        strict :: !(Unify.Type s scope)
      }
  | Disjoint
      { select :: !(Strict.Vector (Select s scope))
      }

data Select s scope
  = Select
  { selectIndex :: !(Strict.Maybe Int),
    constructorInfo :: !(ConstructorInfo s scope)
  }

solve :: SelectorInfo s scope -> Unify.Solve s (Solved.SelectorInfo scope)
solve = \case
  Uniform {position, strict} -> do
    strict <- Unify.solve position strict
    pure Solved.Uniform {strict}
  Disjoint {select} -> do
    select <- traverse solveSelect select
    pure Solved.Disjoint {select}

solveSelect :: Select s scope -> Unify.Solve s (Solved.Select scope)
solveSelect Select {selectIndex, constructorInfo} = do
  constructorInfo <- ConstructorInfo.solve constructorInfo
  pure Solved.Select {selectIndex, constructorInfo}
