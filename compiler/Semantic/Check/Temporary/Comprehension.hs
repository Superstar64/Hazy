module Semantic.Check.Temporary.Comprehension where

import Control.Monad.ST (ST)
import Core.Tree.Type ((#))
import qualified Core.Tree.Type as Core
import Core.Type.Functor (shiftLogical)
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Go.Declarations as Declarations
import {-# SOURCE #-} Semantic.Check.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Expression as Expression
import Semantic.Check.Temporary.Pattern (Pattern)
import qualified Semantic.Check.Temporary.Pattern as Pattern
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Declarations as Proper.Declarations
import qualified Semantic.Tree.Pattern as Semantic.Pattern
import qualified Semantic.Tree.Statements as Semantic
import qualified Semantic.Tree.Statements as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Comprehension s scope
  = Done
      { startPosition :: !Position,
        done :: !(Expression s scope)
      }
  | Run
      { startPosition :: !Position,
        effect :: !(Expression s scope),
        after :: !(Comprehension s scope)
      }
  | Bind
      { startPosition :: !Position,
        patternx :: !(Pattern s scope),
        effect :: !(Expression s scope),
        thenx :: !(Comprehension s (Scope.Pattern ':+ scope)),
        fail :: !Bool
      }
  | Let
      { startPosition :: !Position,
        declarations :: !(Unify.Solve s (Proper.Declarations.Local Group Check scope)),
        body :: !(Comprehension s (Scope.Declaration ':+ scope))
      }

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Statements Semantic.Comprehension Group Resolve scope ->
  ST s (Comprehension s scope)
check context typex = \case
  Semantic.Done {startPosition, done} -> do
    let monad = Core.listing
    inner <- Unify.fresh Core.typex
    done <- Expression.check context inner done
    Unify.unify context startPosition (monad # inner) typex
    pure Done {startPosition, done}
  Semantic.Run {startPosition, effect, after} -> do
    let monad = Core.listing
    kept <- Unify.fresh Core.typex
    Unify.unify context startPosition (monad # kept) typex
    effect <- Expression.check context Core.bool effect
    after <- check context typex after
    pure Run {startPosition, effect, after}
  Semantic.Bind {startPosition, patternx, effect, thenx} -> do
    let monad = Core.listing
    input <- Unify.fresh Core.typex
    output <- Unify.fresh Core.typex
    let neverFail = Semantic.Pattern.neverFails patternx
    Unify.unify context startPosition (monad # output) typex
    patternx <- Pattern.check context input patternx
    effect <- Expression.check context (monad # input) effect
    thenx <- check (Pattern.augment patternx context) (shiftLogical $ monad # output) thenx
    pure Bind {startPosition, patternx, effect, thenx, fail = not neverFail}
  Semantic.Let {startPosition, declarations, body} -> do
    context Declarations.:& declarations <- Declarations.check context declarations
    body <- check context (shiftLogical typex) body
    pure Let {startPosition, declarations, body}

solve :: Comprehension s scope -> Unify.Solve s (Solved.Statements Solved.Comprehension Group Check scope)
solve = \case
  Done {startPosition, done} -> do
    done <- Expression.solve done
    pure Solved.Done {startPosition, done}
  Run {startPosition, effect, after} -> do
    effect <- Expression.solve effect
    after <- solve after
    pure
      Solved.Run
        { startPosition,
          evidence = Solved Semantic.List,
          effect,
          after
        }
  Bind {startPosition, patternx, effect, thenx, fail} -> do
    patternx <- Pattern.solve patternx
    effect <- Expression.solve effect
    thenx <- solve thenx
    pure
      Solved.Bind
        { startPosition,
          patternx,
          evidence = Solved Semantic.List,
          effect,
          thenx,
          fail
        }
  Let {startPosition, declarations, body} -> do
    declarations <- declarations
    body <- solve body
    pure Solved.Let {startPosition, declarations, body}
