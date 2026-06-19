module Generate.Go.Evidence where

import Control.Monad.ST (ST)
import Core.Tree.Evidence (Evidence (..))
import Core.Tree.Instanciation (Instanciation (Instanciation))
import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Generate.Binding.Evidence as Evidence (Binding (..))
import qualified Generate.Binding.Type as Type
import Generate.Context (Context, (!=.))
import qualified Generate.Context as Context
import {-# SOURCE #-} Generate.Go.Expression (force)
import qualified Generate.Mangle as Mangle
import qualified Javascript.Tree.Expression as Javascript (Expression (..))
import qualified Javascript.Tree.Statement as Javascript (Statement (..))
import Semantic.Index.Evidence (Builtin (EqTuple, OrdTuple))
import qualified Semantic.Index.Evidence as Evidence (Builtin (..), Index (..))

generate :: Context s scope -> Evidence scope -> ST s Javascript.Expression
generate context = \case
  Variable {variable = Evidence.Builtin (EqTuple number), instanciation = Instanciation arguments} -> do
    let Mangle.Builtin {eqTuple} = Context.builtin context
    arguments <- traverse (generate context) (toList arguments)
    pure
      Javascript.Call
        { function = Javascript.Variable {name = eqTuple},
          arguments = unpackTuple number : arguments
        }
  Variable {variable = Evidence.Builtin (OrdTuple number), instanciation = Instanciation arguments} -> do
    let Mangle.Builtin {ordTuple} = Context.builtin context
    arguments <- traverse (generate context) (toList arguments)
    pure
      Javascript.Call
        { function = Javascript.Variable {name = ordTuple},
          arguments = unpackTuple number : arguments
        }
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
      Evidence.Builtin evidence -> pure Javascript.Variable {name}
        where
          name = case evidence of
            Evidence.NumInt -> numInt
            Evidence.NumInteger -> numInteger
            Evidence.NumRatio -> numRatio
            Evidence.EnumBool -> enumBool
            Evidence.EnumChar -> enumChar
            Evidence.EnumInt -> enumInt
            Evidence.EnumInteger -> enumInteger
            Evidence.EnumOrdering -> enumOrdering
            Evidence.EnumUnit -> enumUnit
            Evidence.EnumRatio -> enumRatio
            Evidence.EqBool -> eqBool
            Evidence.EqChar -> eqChar
            Evidence.EqInt -> eqInt
            Evidence.EqInteger -> eqInteger
            Evidence.EqList -> eqList
            Evidence.EqOrdering -> eqOrdering
            Evidence.EqRatio -> eqRatio
            Evidence.OrdChar -> ordChar
            Evidence.OrdInt -> ordInt
            Evidence.OrdInteger -> ordInteger
            Evidence.OrdBool -> ordBool
            Evidence.OrdList -> ordList
            Evidence.OrdOrdering -> ordOrdering
            Evidence.OrdRatio -> ordRatio
            Evidence.RealInt -> realInt
            Evidence.RealInteger -> realInteger
            Evidence.RealRatio -> realRatio
            Evidence.IntegralInt -> integralInt
            Evidence.IntegralInteger -> integralInteger
            Evidence.FractionalRatio -> fractionalRatio
            Evidence.FunctorList -> functorList
            Evidence.ApplicativeList -> applicativeList
            Evidence.MonadList -> monadList
            Evidence.MonadFailList -> monadFailList
            Evidence.FunctorST -> functorST
            Evidence.ApplicativeST -> applicativeST
            Evidence.MonadST -> monadST
          Mangle.Builtin
            { numInt,
              numInteger,
              numRatio,
              enumBool,
              enumChar,
              enumInt,
              enumInteger,
              enumOrdering,
              enumUnit,
              enumRatio,
              eqBool,
              eqChar,
              eqInteger,
              eqInt,
              eqList,
              eqOrdering,
              eqRatio,
              ordInt,
              ordInteger,
              ordBool,
              ordChar,
              ordList,
              ordOrdering,
              ordRatio,
              realInt,
              realInteger,
              realRatio,
              integralInt,
              integralInteger,
              fractionalRatio,
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

unpackTuple :: Int -> Javascript.Expression
unpackTuple number =
  Javascript.Arrow
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
