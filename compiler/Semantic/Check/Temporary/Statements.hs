module Semantic.Check.Temporary.Statements where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import Semantic.Check.Temporary.Declarations (Declarations)
import qualified Semantic.Check.Temporary.Declarations as Declarations
import {-# SOURCE #-} Semantic.Check.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Expression as Expression
import Semantic.Check.Temporary.Pattern (Pattern)
import qualified Semantic.Check.Temporary.Pattern as Pattern
import Semantic.Layout (Group)
import Semantic.Locality (Local)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope (Declaration, Pattern)
import Semantic.Shift (shift)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Pattern as Semantic.Pattern
import qualified Semantic.Tree.Statements as Semantic
import qualified Semantic.Tree.Statements as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

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
  Semantic.Statements Semantic.Guard Group Resolve scope ->
  ST s (Statements s scope)
check context typex = \case
  Semantic.Done {done} -> Done <$> Expression.check context typex done
  Semantic.Run {startPosition, effect, after} -> do
    effect <- Expression.check context Unify.bool effect
    after <- check context typex after
    pure Run {startPosition, effect, after}
  Semantic.Bind {startPosition, patternx, effect, thenx} -> do
    binder <- Unify.fresh Unify.typex
    patternx <- Pattern.check context binder patternx
    effect <- Expression.check context binder effect
    thenx <- check (Pattern.augment patternx context) (shift typex) thenx
    pure Bind {startPosition, patternx, effect, thenx}
  Semantic.Let {startPosition, declarations, body} -> do
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
        evidence = Solved Semantic.Bool,
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
        evidence = Solved Semantic.Bool,
        effect,
        thenx,
        fail = not $ Semantic.Pattern.neverFails patternx
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
