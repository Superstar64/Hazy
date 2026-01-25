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
import Stage5.Generate.Context (Context, (!=.))
import qualified Stage5.Generate.Context as Context
import qualified Stage5.Generate.Mangle as Magnle
import qualified Stage5.Generate.Mangle as Mangle

generate :: Context s scope -> Evidence scope -> ST s Javascript.Expression
generate context = \case
  Variable {variable} -> case variable of
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
    evidence -> pure Javacript.Variable {name}
      where
        Mangle.Builtin
          { numInt,
            numInteger,
            enumBool,
            enumChar,
            enumInt,
            enumInteger,
            eqBool,
            eqChar,
            eqInteger,
            eqInt
          } = Context.builtin context
        name = case evidence of
          Evidence.NumInt -> numInt
          Evidence.NumInteger -> numInteger
          Evidence.EnumBool -> enumBool
          Evidence.EnumChar -> enumChar
          Evidence.EnumInt -> enumInt
          Evidence.EnumInteger -> enumInteger
          Evidence.EqBool -> eqBool
          Evidence.EqChar -> eqChar
          Evidence.EqInt -> eqInt
          Evidence.EqInteger -> eqInteger
  Call {function, arguments} -> do
    function <- generate context function
    arguments <- traverse (generate context) (toList arguments)
    pure $
      Javacript.Call
        { function,
          arguments
        }
  Super {base, index} -> do
    base <- generate context base
    pure
      Javascript.Member
        { object = base,
          field = Mangle.supers !! index
        }
