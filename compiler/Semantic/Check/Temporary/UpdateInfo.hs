module Semantic.Check.Temporary.UpdateInfo where

import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Semantic.Check.Simple.UpdateInfo as Solved
import Semantic.Check.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import {-# SOURCE #-} qualified Semantic.Unify as Unify

newtype UpdateInfo s scope = UpdateInfo
  { updateInfo :: Strict.Vector (Update s scope)
  }

data Update s scope = Update
  { constructorInfo :: !(ConstructorInfo s scope),
    selectorIndexes :: !(Strict.Vector (Strict.Maybe Int))
  }

solve :: UpdateInfo s scope -> Unify.Solve s (Solved.UpdateInfo scope)
solve UpdateInfo {updateInfo} = do
  updateInfo <- traverse solveUpdate updateInfo
  pure Solved.UpdateInfo {updateInfo}

solveUpdate :: Update s scope -> Unify.Solve s (Solved.Update scope)
solveUpdate Update {constructorInfo, selectorIndexes} = do
  constructorInfo <- ConstructorInfo.solve constructorInfo
  pure Solved.Update {constructorInfo, selectorIndexes}
