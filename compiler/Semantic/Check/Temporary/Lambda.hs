module Semantic.Check.Temporary.Lambda where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import {-# SOURCE #-} Semantic.Check.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Expression as Expression
import Semantic.Check.Temporary.Pattern (Pattern)
import qualified Semantic.Check.Temporary.Pattern as Pattern
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope (Pattern)
import Semantic.Shift (shift)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Lambda as Semantic
import qualified Semantic.Tree.Lambda as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Lambda s scope
  = Plain
      { plain :: !(Expression s scope)
      }
  | Bound
      { boundPosition :: !Position,
        parameter :: !(Pattern s scope),
        body :: Lambda s (Scope.Pattern ':+ scope)
      }

check :: Context s scope -> Unify.Type s scope -> Semantic.Lambda Group Resolve scope -> ST s (Lambda s scope)
check context typex = \case
  Semantic.Plain {plain} -> do
    plain <- Expression.check context typex plain
    pure Plain {plain}
  Semantic.Bound {boundPosition, parameter, body} -> do
    parameterType <- Unify.fresh Unify.typex
    parameter <- Pattern.check context parameterType parameter
    resultType <- Unify.fresh Unify.typex
    body <- check (Pattern.augment parameter context) (shift resultType) body
    Unify.unify context boundPosition typex (Unify.function parameterType resultType)
    pure Bound {boundPosition, parameter, body}

solve :: Lambda s scope -> Unify.Solve s (Solved.Lambda Group Check scope)
solve = \case
  Plain {plain} -> do
    plain <- Expression.solve plain
    pure Solved.Plain {plain}
  Bound {boundPosition, parameter, body} -> do
    parameter <- Pattern.solve parameter
    body <- solve body
    pure Solved.Bound {boundPosition, parameter, body}
