module Stage2.Check.Temporary.Definition where

import Control.Monad.ST (ST)
import Stage2.Layout (Group)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Definition as Solved
import qualified Stage2.Tree.Definition as Stage2 (Definition (..))
import Stage2.Check.Context (Context)
import Stage2.Check.Temporary.Function (Function)
import qualified Stage2.Check.Temporary.Function as Function
import qualified Stage2.Unify as Unify

data Definition s scope
  = Alternative !(Function s scope) !(Definition s scope)
  | Definition !(Function s scope)

check :: Context s scope -> Unify.Type s scope -> Stage2.Definition Group Resolve scope -> ST s (Definition s scope)
check context typex = \case
  Stage2.Alternative function1 definitions ->
    Alternative <$> Function.check context typex function1 <*> check context typex definitions
  Stage2.Definition function1 -> Definition <$> Function.check context typex function1

solve :: Definition s scope -> Unify.Solve s (Solved.Definition Group Check scope)
solve (Alternative function definition) = do
  function <- Function.solve function
  definition <- solve definition
  pure $ Solved.Alternative function definition
solve (Definition function) = do
  function <- Function.solve function
  pure $ Solved.Definition function
