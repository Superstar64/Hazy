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
          Evidence.Builtin {} -> False
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
      Evidence.Builtin evidence ->
        let unpackTuple number =
              [ Javascript.Arrow
                  { parameters = [Mangle.local],
                    body =
                      [ Javascript.Return $
                          Javascript.Array $
                            do
                              name <- take number $ tail Mangle.names
                              pure $
                                Javascript.Member
                                  { object = Javascript.Variable {name = Mangle.local},
                                    field = name
                                  }
                      ]
                  }
              ]
         in case evidence of
              Evidence.EqTuple number ->
                pure
                  Javascript.Call
                    { function = Javascript.Variable {name = eqTuple},
                      arguments = unpackTuple number
                    }
              Evidence.OrdTuple number ->
                pure
                  Javascript.Call
                    { function = Javascript.Variable {name = ordTuple},
                      arguments = unpackTuple number
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
                    Evidence.EnumOrdering -> enumOrdering
                    Evidence.EnumUnit -> enumUnit
                    Evidence.EqBool -> eqBool
                    Evidence.EqChar -> eqChar
                    Evidence.EqInt -> eqInt
                    Evidence.EqInteger -> eqInteger
                    Evidence.EqList -> eqList
                    Evidence.EqOrdering -> eqOrdering
                    Evidence.OrdChar -> ordChar
                    Evidence.OrdInt -> ordInt
                    Evidence.OrdInteger -> ordInteger
                    Evidence.OrdBool -> ordBool
                    Evidence.OrdList -> ordList
                    Evidence.OrdOrdering -> ordOrdering
                    Evidence.RealInt -> realInt
                    Evidence.RealInteger -> realInteger
                    Evidence.IntegralInt -> integralInt
                    Evidence.IntegralInteger -> integralInteger
                    Evidence.FunctorList -> functorList
                    Evidence.ApplicativeList -> applicativeList
                    Evidence.MonadList -> monadList
                    Evidence.MonadFailList -> monadFailList
                    Evidence.FunctorST -> functorST
                    Evidence.ApplicativeST -> applicativeST
                    Evidence.MonadST -> monadST
        where
          Mangle.Builtin
            { numInt,
              numInteger,
              enumBool,
              enumChar,
              enumInt,
              enumInteger,
              enumOrdering,
              enumUnit,
              eqBool,
              eqChar,
              eqTuple,
              eqInteger,
              eqInt,
              eqList,
              eqOrdering,
              ordInt,
              ordInteger,
              ordBool,
              ordChar,
              ordTuple,
              ordList,
              ordOrdering,
              realInt,
              realInteger,
              integralInt,
              integralInteger,
              functorList,
              applicativeList,
              monadList,
              monadFailList,
              functorST,
              applicativeST,
              monadST
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
