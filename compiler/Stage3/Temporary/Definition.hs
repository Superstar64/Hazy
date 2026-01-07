module Stage3.Temporary.Definition where

import Control.Monad.ST (ST)
import qualified Stage2.Tree.Definition as Stage2 (Definition (..))
import Stage3.Check.Context (Context)
import Stage3.Temporary.Function (Function)
import qualified Stage3.Temporary.Function as Function
import qualified Stage3.Tree.Definition as Solved
import qualified Stage3.Unify as Unify

data Definition s scope
  = Alternative !(Function s scope) !(Definition s scope)
  | Definition !(Function s scope)

check :: Context s scope -> Unify.Type s scope -> Stage2.Definition scope -> ST s (Definition s scope)
check context typex = \case
  Stage2.Alternative function1 definitions ->
    Alternative <$> Function.check context typex function1 <*> check context typex definitions
  Stage2.Definition function1 -> Definition <$> Function.check context typex function1

solve :: Definition s scope -> ST s (Solved.Definition scope)
solve (Alternative function definition) = do
  function <- Function.solve function
  definition <- solve definition
  pure $ Solved.Alternative function definition
solve (Definition function) = do
  function <- Function.solve function
  pure $ Solved.Definition function
