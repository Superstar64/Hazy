module Stage3.Temporary.Statements where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Layout (Group)
import Stage2.Locality (Local)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Declaration, Pattern)
import Stage2.Shift (shift)
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import qualified Stage2.Tree.Pattern as Stage2.Pattern
import qualified Stage2.Tree.Statements as Solved
import qualified Stage2.Tree.Statements as Stage2
import Stage3.Check.Context (Context)
import Stage3.Temporary.Declarations (Declarations)
import qualified Stage3.Temporary.Declarations as Declarations
import {-# SOURCE #-} Stage3.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Stage3.Temporary.Expression as Expression
import Stage3.Temporary.Pattern (Pattern)
import qualified Stage3.Temporary.Pattern as Pattern
import qualified Stage3.Unify as Unify

data Statements s scope
  = Done !(Expression s scope)
  | Run
      { startPosition :: !Position,
        effect :: !(Expression s scope),
        after :: !(Statements s scope)
      }
  | Bind
      { startPosition :: !Position,
        patternx :: !(Pattern s scope),
        effect :: !(Expression s scope),
        thenx :: !(Statements s (Scope.Pattern ':+ scope))
      }
  | Let
      { startPosition :: !Position,
        declarations :: !(Declarations Local s (Scope.Declaration ':+ scope)),
        body :: !(Statements s (Scope.Declaration ':+ scope))
      }

check ::
  Context s scope ->
  Unify.Type s scope ->
  Stage2.Statements Stage2.Guard Group Resolve scope ->
  ST s (Statements s scope)
check context typex = \case
  Stage2.Done {done} -> Done <$> Expression.check context typex done
  Stage2.Run {startPosition, effect, after} -> do
    effect <- Expression.check context Unify.bool effect
    after <- check context typex after
    pure Run {startPosition, effect, after}
  Stage2.Bind {startPosition, patternx, effect, thenx} -> do
    binder <- Unify.fresh Unify.typex
    patternx <- Pattern.check context binder patternx
    effect <- Expression.check context binder effect
    thenx <- check (Pattern.augment patternx context) (shift typex) thenx
    pure Bind {startPosition, patternx, effect, thenx}
  Stage2.Let {startPosition, declarations, body} -> do
    (context, declarations) <- Declarations.check context declarations
    body <- check context (shift typex) body
    pure Let {startPosition, declarations, body}

solve :: Statements s scope -> Unify.Solve s (Solved.Statements Solved.Guard Group Check scope)
solve (Done expression) = do
  expression <- Expression.solve expression
  pure $ Solved.Done expression
solve (Run startPosition expression statements) = do
  effect <- Expression.solve expression
  after <- solve statements
  pure $
    Solved.Run
      { startPosition,
        evidence = Solved Stage2.Bool,
        effect,
        after
      }
solve (Bind startPosition patternx expression statements) = do
  patternx <- Pattern.solve patternx
  effect <- Expression.solve expression
  thenx <- solve statements
  pure $
    Solved.Bind
      { startPosition,
        patternx,
        evidence = Solved Stage2.Bool,
        effect,
        thenx,
        fail = not $ Stage2.Pattern.neverFails patternx
      }
solve (Let startPosition declarations statements) = do
  declarations <- Declarations.solve declarations
  body <- solve statements
  pure $
    Solved.Let
      { startPosition,
        declarations,
        body
      }
