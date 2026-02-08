module Stage3.Temporary.Lambda where

import Control.Monad.ST (ST)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Pattern)
import Stage2.Shift (shift)
import qualified Stage2.Tree.Lambda as Stage2
import Stage3.Check.Context (Context)
import {-# SOURCE #-} Stage3.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Stage3.Temporary.Expression as Expression
import Stage3.Temporary.Pattern (Pattern)
import qualified Stage3.Temporary.Pattern as Pattern
import qualified Stage3.Tree.Lambda as Solved
import qualified Stage3.Unify as Unify

data Lambda s scope
  = Plain
      { plain :: !(Expression s scope)
      }
  | Bound
      { parameter :: !(Pattern s scope),
        body :: Lambda s (Scope.Pattern ':+ scope)
      }

instance Unify.Zonk Lambda where
  zonk zonker = \case
    Plain {plain} -> do
      plain <- Unify.zonk zonker plain
      pure Plain {plain}
    Bound {parameter, body} -> do
      parameter <- Unify.zonk zonker parameter
      body <- Unify.zonk zonker body
      pure Bound {parameter, body}

check :: Context s scope -> Unify.Type s scope -> Stage2.Lambda scope -> ST s (Lambda s scope)
check context typex = \case
  Stage2.Plain {plain} -> do
    plain <- Expression.check context typex plain
    pure Plain {plain}
  Stage2.Bound {boundPosition, parameter, body} -> do
    parameterType <- Unify.fresh Unify.typex
    parameter <- Pattern.check context parameterType parameter
    resultType <- Unify.fresh Unify.typex
    body <- check (Pattern.augment parameter context) (shift resultType) body
    Unify.unify context boundPosition typex (Unify.function parameterType resultType)
    pure Bound {parameter, body}

solve :: Lambda s scope -> ST s (Solved.Lambda scope)
solve = \case
  Plain {plain} -> do
    plain <- Expression.solve plain
    pure Solved.Plain {plain}
  Bound {parameter, body} -> do
    parameter <- Pattern.solve parameter
    body <- solve body
    pure Solved.Bound {parameter, body}
