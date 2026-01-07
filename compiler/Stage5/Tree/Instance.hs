module Stage5.Tree.Instance where

import Control.Monad (zipWithM)
import Control.Monad.ST (ST)
import Data.Foldable (toList)
import qualified Data.Vector as Vector
import qualified Javascript.Tree.Expression as Javascript (Expression (..))
import qualified Javascript.Tree.Field as Javascript (Field (..))
import qualified Javascript.Tree.Statement as Javascript (Statement (..))
import Stage4.Tree.Instance (Instance (..))
import Stage5.Generate.Context (Context)
import qualified Stage5.Generate.Context as Context
import qualified Stage5.Generate.Mangle as Mangle
import qualified Stage5.Tree.Evidence as Evidence
import qualified Stage5.Tree.Expression as Expression

generate :: Context s scope -> Instance scope -> ST s Javascript.Expression
generate context Instance {evidence, prerequisitesCount, memberConstraintCounts, members} = do
  fresh <- Vector.replicateM prerequisitesCount (Context.fresh context)
  context <- pure $ Context.evidenceBindings fresh context
  evidence <- traverse (Evidence.generate context) (toList evidence)
  let supers = Javascript.Literal <$> evidence
  members <-
    zipWithM
      (Expression.declaration context)
      (toList memberConstraintCounts)
      (toList members)
  let fields = Javascript.Literal <$> members
  let object =
        Javascript.Object
          { Javascript.fields = zip Mangle.supers supers <> zip Mangle.fields fields
          }
  if
    | 0 <- prerequisitesCount -> pure object
    | otherwise ->
        pure
          Javascript.Arrow
            { Javascript.parameters = toList fresh,
              Javascript.body = [Javascript.Return object]
            }
