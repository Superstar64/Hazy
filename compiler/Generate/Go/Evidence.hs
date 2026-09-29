module Generate.Go.Evidence where

import Control.Monad.ST (ST)
import Core.Tree.Evidence (Evidence, EvidenceF (..))
import Core.Tree.Instanciation (InstanciationF (Instanciation))
import qualified Core.Tree.Instanciation as Instanciation
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
import Semantic.Index.Evidence (Index (Direct))
import qualified Semantic.Index.Evidence as Evidence (Index (..))
import Semantic.Index.Type2 (Index (..))

generate :: Context s scope -> Evidence scope -> ST s Javascript.Expression
generate context = \case
  Variable {variable = Direct Eq (Tuple number), instanciation}
    | Instanciation arguments <- instanciation -> do
        let Mangle.Builtin {eqTuple} = Context.builtin context
        arguments <- traverse (generate context) (toList arguments)
        pure
          Javascript.Call
            { function = Javascript.Variable {name = eqTuple},
              arguments = unpackTuple number : arguments
            }
    | otherwise -> error "bad tuple instance"
  Variable {variable = Direct Ord (Tuple number), instanciation}
    | Instanciation arguments <- instanciation -> do
        let Mangle.Builtin {ordTuple} = Context.builtin context
        arguments <- traverse (generate context) (toList arguments)
        pure
          Javascript.Call
            { function = Javascript.Variable {name = ordTuple},
              arguments = unpackTuple number : arguments
            }
    | otherwise -> error "bad tuple instance"
  Variable {variable, instanciation} -> do
    let strict = case variable of
          Evidence.Index {} -> True
          Direct {} -> False
    literal@function <- case variable of
      Evidence.Direct target (Index index)
        | Type.Binding {dataInstances} <- context !=. index,
          Just binding <- Map.lookup target dataInstances -> do
            name <- Context.symbol context binding
            pure Javascript.Variable {name}
      Evidence.Direct (Index index) target
        | Type.Binding {classInstances} <- context !=. index,
          Just binding <- Map.lookup target classInstances -> do
            name <- Context.symbol context binding
            pure Javascript.Variable {name}
      Evidence.Index index
        | Evidence.Binding name <- context Context.!~ index ->
            pure Javascript.Variable {name}
      index -> pure Javascript.Variable {name}
        where
          name = case index of
            Direct Num Int -> numInt
            Direct Num Integer -> numInteger
            Direct Num Ratio -> numRatio
            Direct Enum Bool -> enumBool
            Direct Enum Char -> enumChar
            Direct Enum Int -> enumInt
            Direct Enum Integer -> enumInteger
            Direct Enum Ordering -> enumOrdering
            Direct Enum (Tuple 0) -> enumUnit
            Direct Enum Ratio -> enumRatio
            Direct Eq Bool -> eqBool
            Direct Eq Char -> eqChar
            Direct Eq Int -> eqInt
            Direct Eq Integer -> eqInteger
            Direct Eq List -> eqList
            Direct Eq NonEmpty -> eqNonEmpty
            Direct Eq Ordering -> eqOrdering
            Direct Eq Ratio -> eqRatio
            Direct Ord Char -> ordChar
            Direct Ord Int -> ordInt
            Direct Ord Integer -> ordInteger
            Direct Ord Bool -> ordBool
            Direct Ord List -> ordList
            Direct Ord NonEmpty -> ordNonEmpty
            Direct Ord Ordering -> ordOrdering
            Direct Ord Ratio -> ordRatio
            Direct Real Int -> realInt
            Direct Real Integer -> realInteger
            Direct Real Ratio -> realRatio
            Direct Integral Int -> integralInt
            Direct Integral Integer -> integralInteger
            Direct Fractional Ratio -> fractionalRatio
            Direct Functor List -> functorList
            Direct Functor NonEmpty -> functorNonEmpty
            Direct Applicative List -> applicativeList
            Direct Applicative NonEmpty -> applicativeNonEmpty
            Direct Monad List -> monadList
            Direct Monad NonEmpty -> monadNonEmpty
            Direct MonadFail List -> monadFailList
            Direct Functor ST -> functorST
            Direct Applicative ST -> applicativeST
            Direct Monad ST -> monadST
            Direct Semigroup List -> semigroupList
            Direct Semigroup NonEmpty -> semigroupNonEmpty
            Direct Monoid List -> monoidList
            Direct _ _ -> error "bad evidence"
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
              eqNonEmpty,
              eqOrdering,
              eqRatio,
              ordInt,
              ordInteger,
              ordBool,
              ordChar,
              ordList,
              ordNonEmpty,
              ordOrdering,
              ordRatio,
              realInt,
              realInteger,
              realRatio,
              integralInt,
              integralInteger,
              fractionalRatio,
              functorList,
              functorNonEmpty,
              applicativeList,
              applicativeNonEmpty,
              monadList,
              monadNonEmpty,
              monadFailList,
              functorST,
              applicativeST,
              monadST,
              semigroupList,
              semigroupNonEmpty,
              monoidList
            } = Context.builtin context
    case instanciation of
      Instanciation.Mono ->
        pure $
          if strict
            then literal
            else force literal
      Instanciation arguments -> do
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
