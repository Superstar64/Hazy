module Stage5.Tree.Instance where

import Control.Monad.ST (ST)
import Data.Foldable (toList)
import Data.Traversable (for)
import qualified Data.Vector as Vector
import qualified Javascript.Tree.Expression as Javascript (Expression (..))
import qualified Javascript.Tree.Field as Javascript (Field (..))
import qualified Javascript.Tree.Statement as Javascript (Statement (..))
import Stage4.Tree.Instance (Instance (..))
import Stage4.Tree.InstanceMethod (InstanceMethod (..))
import Stage5.Generate.Context (Context)
import qualified Stage5.Generate.Context as Context
import qualified Stage5.Generate.Mangle as Mangle
import qualified Stage5.Tree.Evidence as Evidence
import qualified Stage5.Tree.Expression as Expression

generate :: Context s scope -> Instance scope -> ST s Javascript.Expression
generate context Instance {evidence, prerequisitesCount, members} = do
  fresh <- Vector.replicateM prerequisitesCount (Context.fresh context)
  context <- pure $ Context.evidenceBindings fresh context
  evidence <- traverse (Evidence.generate context) (toList evidence)
  {-
  While super class methods could be plain values, the declaration order
  matters in Javascript if they are generated as that. Instead their are
  generated with a getter to avoid that.

  At some point in the future, values could be generated when it's known that
  the declaration order is safe.
  -}
  let return expression = [Javascript.Return expression]
      supers = Javascript.Getter <$> map return evidence
  members <- for (toList members) $ \method ->
    Expression.declaration context (definition method)
  let fields = Javascript.Literal <$> members
  let object =
        Javascript.Object
          { fields = zip Mangle.fields (supers ++ fields)
          }
  if
    | 0 <- prerequisitesCount -> pure object
    | otherwise ->
        pure
          Javascript.Arrow
            { parameters = toList fresh,
              body = [Javascript.Return object]
            }
