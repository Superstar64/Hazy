module Stage2.Tree.Statements where

import Data.Foldable (toList)
import Stage1.Position (Position)
import qualified Stage1.Tree.Expression as Stage1 (Expression)
import qualified Stage1.Tree.Statement as Stage1 (Statement (..))
import qualified Stage1.Tree.Statements as Stage1 (Statements (..))
import Stage2.Resolve.Context (Context (..))
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope (Declaration, Pattern)
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage2.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage2.Tree.Declarations as Declarations
import {-# SOURCE #-} Stage2.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Stage2.Tree.Expression as Expression (resolve)
import Stage2.Tree.Pattern (Pattern)
import qualified Stage2.Tree.Pattern as Pattern (augment, resolve)

data Statements scope
  = Done
      { done :: !(Expression scope)
      }
  | Run
      { startPosition :: !Position,
        effect :: !(Expression scope),
        after :: !(Statements scope)
      }
  | Bind
      { startPosition :: !Position,
        patternx :: !(Pattern scope),
        effect :: !(Expression scope),
        thenx :: !(Statements (Scope.Pattern ':+ scope))
      }
  | Let
      { startPosition :: !Position,
        declarations :: !(Declarations (Scope.Declaration ':+ scope)),
        body :: !(Statements (Scope.Declaration ':+ scope))
      }
  deriving (Show)

instance Shift Statements where
  shift = shiftDefault

instance Shift.Functor Statements where
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

resolve :: Context scope -> Stage1.Statements Position -> Statements scope
resolve context Stage1.Statements {body, done} = statements context (toList body) done
  where
    statements :: Context scope -> [Stage1.Statement Position] -> Stage1.Expression Position -> Statements scope
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
