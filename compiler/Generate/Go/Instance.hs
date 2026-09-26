module Generate.Go.Instance where

import Control.Monad.ST (ST)
import Core.Tree.Constraints (ConstraintsF (..))
import Core.Tree.EvidenceSet (EvidenceSet (..))
import Core.Tree.Instance (Instance (..))
import Core.Tree.MethodConcrete (MethodConcrete (..))
import Data.Foldable (toList)
import Data.Traversable (for)
import qualified Data.Vector as Vector
import Generate.Context (Context)
import qualified Generate.Context as Context
import qualified Generate.Go.Evidence as Evidence
import qualified Generate.Go.Expression as Expression
import qualified Generate.Mangle as Mangle
import qualified Javascript.Tree.Expression as Javascript (Expression (..))
import qualified Javascript.Tree.Field as Javascript (Field (..))
import qualified Javascript.Tree.Statement as Javascript (Statement (..))

generate :: Context s scope -> Instance scope -> ST s Javascript.Expression
generate context Instance {evidence = EvidenceSet evidence, prerequisites, members} = do
  let count = case prerequisites of
        Constraints count -> length count
        None -> 0
  fresh <- Vector.replicateM count (Context.fresh context)
  context <- pure $ Context.evidenceBindings fresh context
  evidence <- traverse (Evidence.generate context) (toList evidence)
  let supers = Javascript.Literal <$> evidence
  members <- for (toList members) $ \method ->
    Expression.declaration context (definition method)
  let fields = Javascript.Literal <$> members
  let object =
        Javascript.Object
          { fields = zip Mangle.fields (supers ++ fields)
          }
  case prerequisites of
    None ->
      pure $
        Expression.delay
          [ Javascript.Expression $
              Javascript.Assign
                { target = Expression.done,
                  value = object
                }
          ]
    Constraints {} ->
      pure
        Javascript.Arrow
          { parameters = toList fresh,
            body = [Javascript.Return object]
          }
