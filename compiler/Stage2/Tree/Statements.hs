module Stage2.Tree.Statements where

import Data.Foldable (toList)
import Stage1.Position (Position)
import qualified Stage1.Tree.Expression as Stage1 (Expression)
import qualified Stage1.Tree.Statement as Stage1 (Statement (..))
import qualified Stage1.Tree.Statements as Stage1 (Statements (..))
import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (..))
import qualified Stage2.FreeVariables as FreeVariables
import Stage2.Layout (Normal)
import qualified Stage2.Locality as Locality
import Stage2.Resolve.Context (Context (..))
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope (Declaration, Pattern)
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Resolve)
import {-# SOURCE #-} Stage2.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage2.Tree.Declarations as Declarations
import {-# SOURCE #-} Stage2.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Stage2.Tree.Expression as Expression (resolve)
import Stage2.Tree.Pattern (Pattern)
import qualified Stage2.Tree.Pattern as Pattern (augment, resolve)

data Statements layout stage scope
  = Done
      { done :: !(Expression layout stage scope)
      }
  | Run
      { startPosition :: !Position,
        effect :: !(Expression layout stage scope),
        after :: !(Statements layout stage scope)
      }
  | Bind
      { startPosition :: !Position,
        patternx :: !(Pattern Resolve scope),
        effect :: !(Expression layout stage scope),
        thenx :: !(Statements layout stage (Scope.Pattern ':+ scope))
      }
  | Let
      { startPosition :: !Position,
        declarations :: !(Declarations Locality.Local layout stage (Scope.Declaration ':+ scope)),
        body :: !(Statements layout stage (Scope.Declaration ':+ scope))
      }
  deriving (Show)

instance Shift (Statements layout stage) where
  shift = shiftDefault

instance Shift.Functor (Statements layout stage) where
  map category = \case
    Done {done} ->
      Done
        { done = Shift.map category done
        }
    Run {startPosition, effect, after} ->
      Run
        { startPosition,
          effect = Shift.map category effect,
          after = Shift.map category after
        }
    Bind {startPosition, patternx, effect, thenx} ->
      Bind
        { startPosition,
          patternx = Shift.map category patternx,
          effect = Shift.map category effect,
          thenx = Shift.map (Shift.Over category) thenx
        }
    Let {startPosition, declarations, body} ->
      Let
        { startPosition,
          declarations = Shift.map (Shift.Over category) declarations,
          body = Shift.map (Shift.Over category) body
        }

instance FreeTermVariables (Statements layout stage) where
  freeTermVariables target = \case
    Done {done} -> freeTermVariables target done
    Run {effect, after} ->
      concat
        [ freeTermVariables target effect,
          freeTermVariables target after
        ]
    Bind {effect, thenx} ->
      concat
        [ freeTermVariables target effect,
          freeTermVariables (FreeVariables.Over target) thenx
        ]
    Let {declarations, body} ->
      concat
        [ freeTermVariables (FreeVariables.Over target) declarations,
          freeTermVariables (FreeVariables.Over target) body
        ]

instance Connect Statements where
  connect = \case
    Done {done} ->
      Done
        { done = connect done
        }
    Run {startPosition, effect, after} ->
      Run
        { startPosition,
          effect = connect effect,
          after = connect after
        }
    Bind {startPosition, patternx, effect, thenx} ->
      Bind
        { startPosition,
          patternx,
          effect = connect effect,
          thenx = connect thenx
        }
    Let {startPosition, declarations, body} ->
      Let
        { startPosition,
          declarations = Declarations.connect declarations,
          body = connect body
        }

resolve :: Context scope -> Stage1.Statements Position -> Statements Normal Resolve scope
resolve context Stage1.Statements {body, done} = statements context (toList body) done
  where
    statements ::
      Context scope ->
      [Stage1.Statement Position] ->
      Stage1.Expression Position ->
      Statements Normal Resolve scope
    statements context [] done =
      Done
        { done = Expression.resolve context done
        }
    statements context (Stage1.Run {startPosition, expression} : remaining) done =
      Run
        { startPosition,
          effect = Expression.resolve context expression,
          after = statements context remaining done
        }
    statements context (Stage1.Bind {startPosition, patternx, expression} : remaining) done =
      Bind
        { startPosition,
          patternx = pattern',
          effect = Expression.resolve context expression,
          thenx = statements (Pattern.augment pattern' context) remaining done
        }
      where
        pattern' = Pattern.resolve context patternx
    statements context (Stage1.Let {startPosition, declarations} : remaining) done
      | (context, declarations) <- Declarations.resolve context declarations =
          Let
            { startPosition,
              declarations,
              body = statements context remaining done
            }
