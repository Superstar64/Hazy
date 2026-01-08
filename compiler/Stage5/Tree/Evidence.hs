module Stage5.Tree.Evidence where

import Control.Monad.ST (ST)
import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Javascript.Tree.Expression as Javacript
import qualified Javascript.Tree.Expression as Javascript
import qualified Stage2.Index.Type2 as Type2
import qualified Stage3.Index.Evidence as Evidence (Index (..))
import Stage4.Tree.Evidence (Evidence (..))
import qualified Stage5.Generate.Binding.Evidence as Evidence (Binding (..))
import qualified Stage5.Generate.Binding.Type as Type
import Stage5.Generate.Context (Context (Context), (!=.))
import qualified Stage5.Generate.Context as Context
import qualified Stage5.Generate.Mangle as Mangle

generate :: Context s scope -> Evidence scope -> ST s Javascript.Expression
generate
  ~context@Context
    { builtin =
        Context.Builtin
          { numInt,
            numInteger,
            enumInt,
            enumInteger
          }
    } = \case
    Proof {proof, arguments} ->
      if null arguments
        then
          base
        else do
          function <- base
          arguments <- traverse (generate context) (toList arguments)
          pure $
            Javacript.Call
              { function,
                arguments
              }
      where
        base = case proof of
          Evidence.Direct Type2.Num Type2.Int ->
            pure Javascript.Variable {name = numInt}
          Evidence.Direct Type2.Num Type2.Integer ->
            pure Javascript.Variable {name = numInteger}
          Evidence.Direct Type2.Enum Type2.Int ->
            pure Javascript.Variable {name = enumInt}
          Evidence.Direct Type2.Enum Type2.Integer ->
            pure Javascript.Variable {name = enumInteger}
          Evidence.Direct (Type2.Index index) target
            | Type.Binding {classInstances} <- context !=. index,
              Just binding <- Map.lookup target classInstances -> do
                name <- Context.symbol context binding
                pure Javascript.Variable {name}
          Evidence.Direct target (Type2.Index index)
            | Type.Binding {dataInstances} <- context !=. index,
              Just binding <- Map.lookup target dataInstances -> do
                name <- Context.symbol context binding
                pure Javascript.Variable {name}
          Evidence.Index index
            | Evidence.Binding name <- context Context.!~ index ->
                pure Javascript.Variable {name}
          _ -> error "bad evidence"
    Super {base, index} -> do
      base <- generate context base
      pure
        Javascript.Member
          { object = base,
            field = Mangle.supers !! index
          }
