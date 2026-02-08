module Stage3.Temporary.Body where

import Control.Monad.ST (ST)
import qualified Data.Strict.Vector1 as Strict
import qualified Stage2.Tree.Body as Stage2 (Body (..))
import Stage3.Check.Context (Context)
import {-# SOURCE #-} Stage3.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Stage3.Temporary.Expression as Expression
import {-# SOURCE #-} Stage3.Temporary.Statements (Statements)
import {-# SOURCE #-} qualified Stage3.Temporary.Statements as Statements
import qualified Stage3.Tree.Body as Solved
import qualified Stage3.Unify as Unify

data Body s scope
  = Body !(Expression s scope)
  | Guards !(Strict.Vector1 (Statements s scope))

instance Unify.Zonk Body where
  zonk zonker = \case
    Body expression -> Body <$> Unify.zonk zonker expression
    Guards statements -> do
      statements <- traverse (Unify.zonk zonker) statements
      pure $ Guards statements

check :: Context s scope -> Unify.Type s scope -> Stage2.Body scope -> ST s (Body s scope)
check context typex = \case
  Stage2.Body expression1 -> Body <$> Expression.check context typex expression1
  Stage2.Guards statements -> Guards <$> traverse (Statements.check context typex) statements

solve :: Body s scope -> ST s (Solved.Body scope)
solve (Body expression) = do
  expression <- Expression.solve expression
  pure (Solved.Body expression)
solve (Guards statements) = do
  statements <- traverse Statements.solve statements
  pure $ Solved.Guards statements
