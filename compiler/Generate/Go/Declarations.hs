module Generate.Go.Declarations where

import Control.Monad.ST (ST)
import Core.Tree.Declaration (Declaration (..))
import Core.Tree.Declarations (Declarations (..))
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Traversable (for)
import qualified Data.Vector as Vector
import Generate.Context (Context)
import qualified Generate.Context as Context
import qualified Generate.Go.Expression as Expression
import qualified Generate.Go.Instance as Instance
import Generate.LocalType (LocalType (LocalType))
import qualified Generate.LocalType as LocalType
import qualified Javascript.Tree.Statement as Javascript
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope

generate ::
  Context s scope ->
  Declarations (Scope.Declaration ':+ scope) ->
  ST s (Context s (Scope.Declaration ':+ scope), [Javascript.Statement local])
generate context Declarations {terms, classInstances, dataInstances} = do
  variables <- Vector.replicateM (length terms) (Context.fresh context)
  classVariables <- traverse (traverse $ const $ Context.fresh context) classInstances
  dataVariables <- traverse (traverse $ const $ Context.fresh context) dataInstances
  let instances = Vector.zipWith localType classVariables dataVariables
      localType classInstances dataInstances =
        LocalType {classInstances, dataInstances}
  context <- pure $ Context.localBindings variables instances context
  statements <- for (zip (toList variables) (toList terms)) $
    \(name, Declaration {definition}) -> do
      thunk <- Expression.declaration context definition
      pure $ Javascript.Const name thunk
  classStatements <-
    for (zip (toList classVariables) (toList classInstances)) $
      \(names, instances) -> for (Map.toList instances) $ \(index, instancex) -> do
        let name = names Map.! index
        instancex <- Instance.generate context instancex
        pure $ Javascript.Const name instancex
  dataStatements <-
    for (zip (toList dataVariables) (toList dataInstances)) $
      \(names, instances) -> for (Map.toList instances) $ \(index, instancex) -> do
        let name = names Map.! index
        instancex <- Instance.generate context instancex
        pure $ Javascript.Const name instancex
  pure (context, statements ++ concat classStatements ++ concat dataStatements)
