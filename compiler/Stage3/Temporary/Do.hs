module Stage3.Temporary.Do where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (shift)
import qualified Stage2.Tree.Pattern as Stage2.Pattern
import qualified Stage2.Tree.Statements as Stage2
import Stage3.Check.Context (Context)
import Stage3.Temporary.Declarations (Declarations)
import qualified Stage3.Temporary.Declarations as Declarations
import {-# SOURCE #-} Stage3.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Stage3.Temporary.Expression as Expression
import Stage3.Temporary.Pattern (Pattern)
import qualified Stage3.Temporary.Pattern as Pattern
import qualified Stage3.Tree.Do as Solved
import qualified Stage3.Unify as Unify

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
      { declarations :: !(Declarations s (Scope.Declaration ':+ scope)),
        letBody :: !(Do s (Scope.Declaration ':+ scope))
      }

instance Unify.Zonk Do where
  zonk zonker = \case
    Done {done} -> do
      done <- Unify.zonk zonker done
      pure Done {done}
    Run {startPosition, evidence, effect, after} -> do
      evidence <- Unify.zonk zonker evidence
      effect <- Unify.zonk zonker effect
      after <- Unify.zonk zonker after
      pure Run {startPosition, evidence, effect, after}
    Bind {startPosition, patternx, evidence, effect, thenx, fail} -> do
      patternx <- Unify.zonk zonker patternx
      evidence <- Unify.zonk zonker evidence
      effect <- Unify.zonk zonker effect
      thenx <- Unify.zonk zonker thenx
      pure Bind {startPosition, patternx, evidence, effect, thenx, fail}
    Let {declarations, letBody} -> do
      declarations <- Unify.zonk zonker declarations
      letBody <- Unify.zonk zonker letBody
      pure Let {declarations, letBody}

check :: Context s scope -> Unify.Type s scope -> Stage2.Statements scope -> ST s (Do s scope)
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
  Stage2.Let {declarations, body} -> do
    (context, declarations) <- Declarations.check context declarations
    body <- check context (shift typex) body
    pure (Let declarations body)

solve :: Do s scope -> ST s (Solved.Do scope)
solve = \case
  Done {done} -> do
    done <- Expression.solve done
    pure Solved.Done {done}
  Run {startPosition, evidence, effect, after} -> do
    evidence <- Unify.solveEvidence startPosition evidence
    effect <- Expression.solve effect
    after <- solve after
    pure Solved.Run {evidence, effect, after}
  Bind {startPosition, patternx, evidence, effect, thenx, fail} -> do
    patternx <- Pattern.solve patternx
    evidence <- Unify.solveEvidence startPosition evidence
    effect <- Expression.solve effect
    thenx <- solve thenx
    pure Solved.Bind {patternx, evidence, effect, thenx, fail}
  Let {declarations, letBody} -> do
    declarations <- Declarations.solve declarations
    letBody <- solve letBody
    pure Solved.Let {declarations, letBody}
