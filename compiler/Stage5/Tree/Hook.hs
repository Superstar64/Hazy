module Stage5.Tree.Hook where

import Control.Monad.ST (ST)
import qualified Javascript.Tree.Expression as Javascript (Expression (..))
import qualified Javascript.Tree.Statement as Javascript (Statement (..))
import Stage2.Index.Method (Applicative (..), Enum (..), Eq (..), Functor (..), Monad (..), MonadFail (..), Num (..))
import Stage4.Tree.Hook (Hook (..))
import Stage5.Generate.Context (Context (..))
import Stage5.Generate.Mangle (Builtin (..))
import qualified Stage5.Tree.Evidence as Evidence

generateInto :: Context s scope -> Javascript.Expression -> Hook scope -> ST s [Javascript.Statement local]
generateInto context target hook = do
  let Context {builtin} = context
      Builtin
        { defaultPlus,
          defaultMinus,
          defaultMultiply,
          defaultNegate,
          defaultAbs,
          defaultSignum,
          defaultFromInteger,
          defaultSucc,
          defaultPred,
          defaultToEnum,
          defaultFromEnum,
          defaultEnumFrom,
          defaultEnumFromThen,
          defaultEnumFromTo,
          defaultEnumFromThenTo,
          defaultEqual,
          defaultNotEqual,
          defaultFmap,
          defaultFconst,
          defaultPure,
          defaultAp,
          defaultLiftA2,
          defaultDiscardLeft,
          defaultDiscardRight,
          defaultBind,
          defaultThen,
          defaultReturn,
          defaultFail
        } = builtin
      defaultx evidence name = do
        evidence <- Evidence.generate context evidence
        pure
          Javascript.Call
            { function = Javascript.Variable {name},
              arguments = [evidence]
            }
  expression <- case hook of
    DefaultNum {evidence, num} -> defaultx evidence $ case num of
      Plus -> defaultPlus
      Minus -> defaultMinus
      Multiply -> defaultMultiply
      Negate -> defaultNegate
      Abs -> defaultAbs
      Signum -> defaultSignum
      FromInteger -> defaultFromInteger
    DefaultEnum {evidence, enum} -> defaultx evidence $ case enum of
      Succ -> defaultSucc
      Pred -> defaultPred
      ToEnum -> defaultToEnum
      FromEnum -> defaultFromEnum
      EnumFrom -> defaultEnumFrom
      EnumFromThen -> defaultEnumFromThen
      EnumFromTo -> defaultEnumFromTo
      EnumFromThenTo -> defaultEnumFromThenTo
    DefaultEq {evidence, eq} -> defaultx evidence $ case eq of
      Equal -> defaultEqual
      NotEqual -> defaultNotEqual
    DefaultFunctor {evidence, functor} -> defaultx evidence $ case functor of
      Fmap -> defaultFmap
      Fconst -> defaultFconst
    DefaultApplicative {evidence, applicative} -> defaultx evidence $ case applicative of
      Pure -> defaultPure
      Ap -> defaultAp
      LiftA2 -> defaultLiftA2
      DiscardLeft -> defaultDiscardLeft
      DiscardRight -> defaultDiscardRight
    DefaultMonad {evidence, monad} -> defaultx evidence $ case monad of
      Bind -> defaultBind
      Then -> defaultThen
      Return -> defaultReturn
    DefaultMonadFail {evidence, monadFail} -> defaultx evidence $ case monadFail of
      Fail -> defaultFail

  pure [done expression]
  where
    done value =
      Javascript.Expression
        ( Javascript.Assign
            { target,
              value
            }
        )
