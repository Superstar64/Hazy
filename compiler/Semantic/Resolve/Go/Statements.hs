module Semantic.Resolve.Go.Statements where

import Data.Foldable (toList)
import Semantic.Layout (Normal)
import Semantic.Resolve.Context (Context (..))
import {-# SOURCE #-} Semantic.Resolve.Go.Declarations as Declarations (resolve)
import {-# SOURCE #-} qualified Semantic.Resolve.Go.Expression as Expression (resolve)
import qualified Semantic.Resolve.Go.Pattern as Pattern (augment, resolve)
import Semantic.Stage (Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Pattern as Pattern (neverFails)
import Semantic.Tree.Statements (Statements (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.Expression as Syntax (Expression)
import qualified Syntax.Tree.Statement as Syntax (Statement (..))
import qualified Syntax.Tree.Statements as Syntax (Statements (..))

resolve :: Context scope -> Syntax.Statements Position -> Statements syntax Normal Resolve scope
resolve context Syntax.Statements {body, done} = statements context (toList body) done
  where
    statements ::
      Context scope ->
      [Syntax.Statement Position] ->
      Syntax.Expression Position ->
      Statements syntax Normal Resolve scope
    statements context [] done =
      Done
        { done = Expression.resolve context done
        }
    statements context (Syntax.Run {startPosition, expression} : remaining) done =
      Run
        { startPosition,
          evidence = Inferred,
          effect = Expression.resolve context expression,
          after = statements context remaining done
        }
    statements context (Syntax.Bind {startPosition, patternx, expression} : remaining) done =
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
    statements context (Syntax.Let {startPosition, declarations} : remaining) done
      | (context, declarations) <- Declarations.resolve context declarations =
          Let
            { startPosition,
              declarations,
              body = statements context remaining done
            }
