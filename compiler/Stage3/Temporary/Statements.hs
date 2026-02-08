module Stage3.Temporary.Statements where

import Control.Monad.ST (ST)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Declaration, Pattern)
import Stage2.Shift (shift)
import qualified Stage2.Tree.Statements as Stage2
import Stage3.Check.Context (Context)
import Stage3.Temporary.Declarations (Declarations)
import qualified Stage3.Temporary.Declarations as Declarations
import {-# SOURCE #-} Stage3.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Stage3.Temporary.Expression as Expression
import Stage3.Temporary.Pattern (Pattern)
import qualified Stage3.Temporary.Pattern as Pattern
import qualified Stage3.Tree.Statements as Solved
import qualified Stage3.Unify as Unify

data Statements s scope
  = Done !(Expression s scope)
  | Run !(Expression s scope) !(Statements s scope)
  | Bind !(Pattern s scope) !(Expression s scope) !(Statements s (Scope.Pattern ':+ scope))
  | Let !(Declarations s (Scope.Declaration ':+ scope)) !(Statements s (Scope.Declaration ':+ scope))

instance Unify.Zonk Statements where
  zonk zonker = \case
    Done expression -> Done <$> Unify.zonk zonker expression
    Run expression statements -> do
      expression <- Unify.zonk zonker expression
      statements <- Unify.zonk zonker statements
      pure $ Run expression statements
    Bind patternx expression statements -> do
      patternx <- Unify.zonk zonker patternx
      expression <- Unify.zonk zonker expression
      statements <- Unify.zonk zonker statements
      pure $ Bind patternx expression statements
    Let declarations statements -> do
      declarations <- Unify.zonk zonker declarations
      statements <- Unify.zonk zonker statements
      pure $ Let declarations statements

check :: Context s scope -> Unify.Type s scope -> Stage2.Statements scope -> ST s (Statements s scope)
check context typex = \case
  Stage2.Done expression -> Done <$> Expression.check context typex expression
  Stage2.Run expression statements -> do
    expression <- Expression.check context Unify.bool expression
    statements <- check context typex statements
    pure $ Run expression statements
  Stage2.Bind patternx expression statements -> do
    binder <- Unify.fresh Unify.typex
    patternx <- Pattern.check context binder patternx
    expression <- Expression.check context binder expression
    statements <- check (Pattern.augment patternx context) (shift typex) statements
    pure $ Bind patternx expression statements
  Stage2.Let declarations statements -> do
    (context, declarations) <- Declarations.check context declarations
    letBody <- check context (shift typex) statements
    pure (Let declarations letBody)

solve :: Statements s scope -> ST s (Solved.Statements scope)
solve (Done expression) = do
  expression <- Expression.solve expression
  pure $ Solved.Done expression
solve (Run expression statements) = do
  expression <- Expression.solve expression
  statements <- solve statements
  pure $ Solved.Run expression statements
solve (Bind patternx expression statements) = do
  patternx <- Pattern.solve patternx
  expression <- Expression.solve expression
  statements <- solve statements
  pure $ Solved.Bind patternx expression statements
solve (Let declarations statements) = do
  declarations <- Declarations.solve declarations
  statements <- solve statements
  pure $ Solved.Let declarations statements
