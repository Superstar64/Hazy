module Stage3.Temporary.SelectorInfo where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import Stage3.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Stage3.Temporary.ConstructorInfo as ConstructorInfo
import qualified Stage3.Tree.SelectorInfo as Solved
import {-# SOURCE #-} qualified Stage3.Unify as Unify

data SelectorInfo s scope
  = Uniform
      { position :: !Position,
        strict :: !(Unify.Type s scope)
      }
  | Disjoint
      { select :: !(Strict.Vector (Select s scope))
      }

instance Unify.Zonk SelectorInfo where
  zonk zonker = \case
    Uniform {position, strict} -> do
      strict <- Unify.zonk zonker strict
      pure Uniform {position, strict}
    Disjoint {select} -> do
      select <- traverse (Unify.zonk zonker) select
      pure Disjoint {select}

data Select s scope
  = Select
  { selectIndex :: !(Strict.Maybe Int),
    constructorInfo :: !(ConstructorInfo s scope)
  }

instance Unify.Zonk Select where
  zonk zonker Select {selectIndex, constructorInfo} = do
    constructorInfo <- Unify.zonk zonker constructorInfo
    pure Select {selectIndex, constructorInfo}

solve :: SelectorInfo s scope -> ST s (Solved.SelectorInfo scope)
solve = \case
  Uniform {position, strict} -> do
    strict <- Unify.solve position strict
    pure Solved.Uniform {strict}
  Disjoint {select} -> do
    select <- traverse solveSelect select
    pure Solved.Disjoint {select}

solveSelect :: Select s scope -> ST s (Solved.Select scope)
solveSelect Select {selectIndex, constructorInfo} = do
  constructorInfo <- ConstructorInfo.solve constructorInfo
  pure Solved.Select {selectIndex, constructorInfo}
