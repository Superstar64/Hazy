module Stage5.Tree.Declarations where

import Control.Monad.ST (ST)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Traversable (for)
import qualified Data.Vector as Vector
import qualified Javascript.Tree.Statement as Javascript
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage4.Tree.Declarations (Declarations (..))
import Stage4.Tree.TermDeclaration (TermDeclaration (Definition))
import qualified Stage4.Tree.TermDeclaration as TermDeclaration
import Stage5.Generate.Context (Context)
import qualified Stage5.Generate.Context as Context
import Stage5.Generate.LocalType (LocalType (LocalType))
import qualified Stage5.Generate.LocalType as LocalType
import qualified Stage5.Tree.Expression as Expression
import qualified Stage5.Tree.Instance as Instance

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
    \(name, Definition {definition}) -> do
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
