module Semantic.Check.Temporary.Body where

import Control.Monad.ST (ST)
import qualified Data.Strict.Vector1 as Strict
import Semantic.Check.Context (Context)
import {-# SOURCE #-} Semantic.Check.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Expression as Expression
import {-# SOURCE #-} Semantic.Check.Temporary.Statements (Statements)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Statements as Statements
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Body as Semantic (Body (..))
import qualified Semantic.Tree.Body as Solved
import qualified Semantic.Unify as Unify

data Body s scope
  = Body !(Expression s scope)
  | Guards !(Strict.Vector1 (Statements s scope))

check :: Context s scope -> Unify.Type s scope -> Semantic.Body Group Resolve scope -> ST s (Body s scope)
check context typex = \case
  Semantic.Body expression1 -> Body <$> Expression.check context typex expression1
  Semantic.Guards statements -> Guards <$> traverse (Statements.check context typex) statements

solve :: Body s scope -> Unify.Solve s (Solved.Body Group Check scope)
solve (Body expression) = do
  expression <- Expression.solve expression
  pure (Solved.Body expression)
solve (Guards statements) = do
  statements <- traverse Statements.solve statements
  pure $ Solved.Guards statements
