module Stage2.Resolve.Go.Statements where

import Data.Foldable (toList)
import Stage1.Position (Position)
import qualified Stage1.Tree.Expression as Stage1 (Expression)
import qualified Stage1.Tree.Statement as Stage1 (Statement (..))
import qualified Stage1.Tree.Statements as Stage1 (Statements (..))
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context (..))
import {-# SOURCE #-} Stage2.Resolve.Go.Declarations as Declarations (resolve)
import {-# SOURCE #-} qualified Stage2.Resolve.Go.Expression as Expression (resolve)
import qualified Stage2.Resolve.Go.Pattern as Pattern (augment, resolve)
import Stage2.Stage (Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import qualified Stage2.Tree.Pattern as Pattern (neverFails)
import Stage2.Tree.Statements (Statements (..))

resolve :: Context scope -> Stage1.Statements Position -> Statements syntax Normal Resolve scope
resolve context Stage1.Statements {body, done} = statements context (toList body) done
  where
    statements ::
      Context scope ->
      [Stage1.Statement Position] ->
      Stage1.Expression Position ->
      Statements syntax Normal Resolve scope
    statements context [] done =
      Done
        { done = Expression.resolve context done
        }
    statements context (Stage1.Run {startPosition, expression} : remaining) done =
      Run
        { startPosition,
          evidence = Inferred,
          effect = Expression.resolve context expression,
          after = statements context remaining done
        }
    statements context (Stage1.Bind {startPosition, patternx, expression} : remaining) done =
      Bind
        { startPosition,
          evidence = Inferred,
          patternx = pattern',
          effect = Expression.resolve context expression,
          thenx = statements (Pattern.augment pattern' context) remaining done,
          fail = not $ Pattern.neverFails pattern'
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
