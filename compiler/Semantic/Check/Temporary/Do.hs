module Semantic.Check.Temporary.Do where

import Control.Monad.ST (ST)
import Core.Tree.Type ((#), (-#>))
import qualified Core.Tree.Type as Core
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Temporary.Declarations as Declarations
import {-# SOURCE #-} Semantic.Check.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Expression as Expression
import Semantic.Check.Temporary.Pattern (Pattern)
import qualified Semantic.Check.Temporary.Pattern as Pattern
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Shift (shift)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Pattern as Semantic.Pattern
import qualified Semantic.Tree.Statements as Semantic
import qualified Semantic.Tree.Statements as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Do s scope
  = Done
      { startPosition :: !Position,
        done :: !(Expression s scope)
      }
  | Run
      { startPosition :: !Position,
        evidence :: !(Unify.Evidence s scope),
        effect :: !(Expression s scope),
        after :: !(Do s scope)
      }
  | Bind
      { startPosition :: !Position,
        patternx :: !(Pattern s scope),
        evidence :: !(Unify.Evidence s scope),
        effect :: !(Expression s scope),
        thenx :: !(Do s (Scope.Pattern ':+ scope)),
        fail :: !Bool
      }
  | Let
      { startPosition :: !Position,
        declarations :: !(Declarations.Local s scope),
        body :: !(Do s (Scope.Declaration ':+ scope))
      }

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Statements Semantic.Do Group Resolve scope ->
  ST s (Do s scope)
check context typex = \case
  Semantic.Done {startPosition, done} -> do
    done <- Expression.check context typex done
    pure Done {startPosition, done}
  Semantic.Run {startPosition, effect, after} -> do
    monad <- Unify.fresh $ Core.typex -#> Core.typex
    evidence <- Unify.constrain context startPosition Type2.Monad monad
    ignore <- Unify.fresh Core.typex
    kept <- Unify.fresh Core.typex
    Unify.unify context startPosition (monad # kept) typex
    effect <- Expression.check context (monad # ignore) effect
    after <- check context typex after
    pure Run {startPosition, evidence, effect, after}
  Semantic.Bind {startPosition, patternx, effect, thenx} -> do
    monad <- Unify.fresh $ Core.typex -#> Core.typex
    input <- Unify.fresh Core.typex
    output <- Unify.fresh Core.typex
    let neverFail = Semantic.Pattern.neverFails patternx
        classx
          | neverFail = Type2.Monad
          | otherwise = Type2.MonadFail
    evidence <- Unify.constrain context startPosition classx monad
    Unify.unify context startPosition (monad # output) typex
    patternx <- Pattern.check context input patternx
    effect <- Expression.check context (monad # input) effect
    thenx <- check (Pattern.augment patternx context) (shift $ monad # output) thenx
    pure Bind {startPosition, patternx, evidence, effect, thenx, fail = not neverFail}
  Semantic.Let {startPosition, declarations, body} -> do
    (context, declarations) <- Declarations.check context declarations
    body <- check context (shift typex) body
    pure Let {startPosition, declarations, body}

solve :: Do s scope -> Unify.Solve s (Solved.Statements Solved.Do Group Check scope)
solve = \case
  Done {startPosition, done} -> do
    done <- Expression.solve done
    pure Solved.Done {startPosition, done}
  Run {startPosition, evidence, effect, after} -> do
    evidence <- Unify.solveEvidence startPosition evidence
    effect <- Expression.solve effect
    after <- solve after
    pure
      Solved.Run
        { startPosition,
          evidence = Solved $ Semantic.Monad evidence,
          effect,
          after
        }
  Bind {startPosition, patternx, evidence, effect, thenx, fail} -> do
    patternx <- Pattern.solve patternx
    evidence <- Unify.solveEvidence startPosition evidence
    effect <- Expression.solve effect
    thenx <- solve thenx
    pure
      Solved.Bind
        { startPosition,
          patternx,
          evidence = Solved $ Semantic.Monad evidence,
          effect,
          thenx,
          fail
        }
  Let {startPosition, declarations, body} -> do
    declarations <- Declarations.solveLocal declarations
    body <- solve body
    pure Solved.Let {startPosition, declarations, body}
