module Stage5.Tree.Evidence where

import Control.Monad.ST (ST)
import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Javascript.Tree.Expression as Javacript
import qualified Javascript.Tree.Expression as Javascript
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
          Evidence.NumInt ->
            pure Javascript.Variable {name = numInt}
          Evidence.NumInteger ->
            pure Javascript.Variable {name = numInteger}
          Evidence.EnumInt ->
            pure Javascript.Variable {name = enumInt}
          Evidence.EnumInteger ->
            pure Javascript.Variable {name = enumInteger}
          Evidence.Class index target
            | Type.Binding {classInstances} <- context !=. index,
              Just binding <- Map.lookup target classInstances -> do
                name <- Context.symbol context binding
                pure Javascript.Variable {name}
            | otherwise -> error "bad evidence"
          Evidence.Data target index
            | Type.Binding {dataInstances} <- context !=. index,
              Just binding <- Map.lookup target dataInstances -> do
                name <- Context.symbol context binding
                pure Javascript.Variable {name}
            | otherwise -> error "bad evidence"
          Evidence.Index index
            | Evidence.Binding name <- context Context.!~ index ->
                pure Javascript.Variable {name}
    Super {base, index} -> do
      base <- generate context base
      pure
        Javascript.Member
          { object = base,
            field = Mangle.supers !! index
          }
