module Stage2.Check.Temporary.Do where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Layout (Group)
import Stage2.Locality (Local)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (shift)
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import qualified Stage2.Tree.Pattern as Stage2.Pattern
import qualified Stage2.Tree.Statements as Solved
import qualified Stage2.Tree.Statements as Stage2
import Stage2.Check.Context (Context)
import Stage2.Check.Temporary.Declarations (Declarations)
import qualified Stage2.Check.Temporary.Declarations as Declarations
import {-# SOURCE #-} Stage2.Check.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Stage2.Check.Temporary.Expression as Expression
import Stage2.Check.Temporary.Pattern (Pattern)
import qualified Stage2.Check.Temporary.Pattern as Pattern
import qualified Stage2.Unify as Unify

data Do s scope
  = Done {done :: !(Expression s scope)}
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
        declarations :: !(Declarations Local s (Scope.Declaration ':+ scope)),
        body :: !(Do s (Scope.Declaration ':+ scope))
      }

check ::
  Context s scope ->
  Unify.Type s scope ->
  Stage2.Statements Stage2.Do Group Resolve scope ->
  ST s (Do s scope)
check context typex = \case
  Stage2.Done {done} -> do
    done <- Expression.check context typex done
    pure Done {done}
  Stage2.Run {startPosition, effect, after} -> do
    monad <- Unify.fresh $ Unify.typex `Unify.function` Unify.typex
    evidence <- Unify.constrain context startPosition Type2.Monad monad
    ignore <- Unify.fresh Unify.typex
    kept <- Unify.fresh Unify.typex
    Unify.unify context startPosition (Unify.call monad kept) typex
    effect <- Expression.check context (Unify.call monad ignore) effect
    after <- check context typex after
    pure Run {startPosition, evidence, effect, after}
  Stage2.Bind {startPosition, patternx, effect, thenx} -> do
    monad <- Unify.fresh $ Unify.typex `Unify.function` Unify.typex
    input <- Unify.fresh Unify.typex
    output <- Unify.fresh Unify.typex
    let neverFail = Stage2.Pattern.neverFails patternx
        classx
          | neverFail = Type2.Monad
          | otherwise = Type2.MonadFail
    evidence <- Unify.constrain context startPosition classx monad
    Unify.unify context startPosition (Unify.call monad output) typex
    patternx <- Pattern.check context input patternx
    effect <- Expression.check context (Unify.call monad input) effect
    thenx <- check (Pattern.augment patternx context) (shift $ Unify.call monad output) thenx
    pure Bind {startPosition, patternx, evidence, effect, thenx, fail = not neverFail}
  Stage2.Let {startPosition, declarations, body} -> do
    (context, declarations) <- Declarations.check context declarations
    body <- check context (shift typex) body
    pure Let {startPosition, declarations, body}

solve :: Do s scope -> Unify.Solve s (Solved.Statements Solved.Do Group Check scope)
solve = \case
  Done {done} -> do
    done <- Expression.solve done
    pure Solved.Done {done}
  Run {startPosition, evidence, effect, after} -> do
    evidence <- Unify.solveEvidence startPosition evidence
    effect <- Expression.solve effect
    after <- solve after
    pure
      Solved.Run
        { startPosition,
          evidence = Solved $ Stage2.Monad evidence,
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
          evidence = Solved $ Stage2.Monad evidence,
          effect,
          thenx,
          fail
        }
  Let {startPosition, declarations, body} -> do
    declarations <- Declarations.solve declarations
    body <- solve body
    pure Solved.Let {startPosition, declarations, body}
