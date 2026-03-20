module Stage5.Tree.Evidence where

import Control.Monad.ST (ST)
import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Javascript.Tree.Expression as Javascript (Expression (..))
import qualified Javascript.Tree.Statement as Javascript (Statement (..))
import qualified Stage3.Index.Evidence as Evidence (Builtin (..), Index (..))
import Stage4.Tree.Evidence (Evidence (..))
import Stage4.Tree.Instanciation (Instanciation (Instanciation))
import qualified Stage5.Generate.Binding.Evidence as Evidence (Binding (..))
import qualified Stage5.Generate.Binding.Type as Type
import Stage5.Generate.Context (Context, (!=.))
import qualified Stage5.Generate.Context as Context
import qualified Stage5.Generate.Mangle as Mangle
import {-# SOURCE #-} Stage5.Tree.Expression (force)

generate :: Context s scope -> Evidence scope -> ST s Javascript.Expression
generate context = \case
  Variable {variable, instanciation = Instanciation arguments} -> do
    let strict = case variable of
          Evidence.Index {} -> True
          Evidence.Class {} -> False
          Evidence.Data {} -> False
          Evidence.Builtin {} -> True
    literal@function <- case variable of
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
      Evidence.Builtin evidence -> case evidence of
        Evidence.EqTuple number ->
          pure
            Javascript.Call
              { function = Javascript.Variable {name = eqTuple},
                arguments = do
                  name <- take number $ tail Mangle.names
                  pure $
                    Javascript.Arrow
                      { parameters = [Mangle.local],
                        body =
                          [ Javascript.Return $
                              Javascript.Member
                                { object = Javascript.Variable {name = Mangle.local},
                                  field = name
                                }
                          ]
                      }
              }
        _ -> pure Javascript.Variable {name}
          where
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
              Evidence.FunctorList -> functorList
              Evidence.ApplicativeList -> applicativeList
              Evidence.MonadList -> monadList
              Evidence.MonadFailList -> monadFailList
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
              eqTuple,
              eqInteger,
              eqInt,
              functorList,
              applicativeList,
              monadList,
              monadFailList
            } = Context.builtin context
    if null arguments
      then
        pure $
          if strict
            then literal
            else force literal
      else do
        arguments <- traverse (generate context) (toList arguments)
        pure $
          Javascript.Call
            { function,
              arguments
            }
  Super {base, index} -> do
    base <- generate context base
    pure
      Javascript.Member
        { object = base,
          field = Mangle.fields !! index
        }
