module Semantic.Check.Temporary.Definition where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import Semantic.Check.Temporary.Function (Function)
import qualified Semantic.Check.Temporary.Function as Function
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Definition as Semantic (Definition (..))
import qualified Semantic.Tree.Definition as Solved
import qualified Semantic.Unify as Unify

data Definition s scope
  = Alternative !(Function s scope) !(Definition s scope)
  | Definition !(Function s scope)

check :: Context s scope -> Unify.Type s scope -> Semantic.Definition Group Resolve scope -> ST s (Definition s scope)
check context typex = \case
  Semantic.Alternative function1 definitions ->
    Alternative <$> Function.check context typex function1 <*> check context typex definitions
  Semantic.Definition function1 -> Definition <$> Function.check context typex function1

solve :: Definition s scope -> Unify.Solve s (Solved.Definition Group Check scope)
solve (Alternative function definition) = do
  function <- Function.solve function
  definition <- solve definition
  pure $ Solved.Alternative function definition
solve (Definition function) = do
  function <- Function.solve function
  pure $ Solved.Definition function
