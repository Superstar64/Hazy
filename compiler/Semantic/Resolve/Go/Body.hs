module Semantic.Resolve.Go.Body where

import Semantic.Layout (Normal)
import Semantic.Resolve.Context (Context)
import {-# SOURCE #-} qualified Semantic.Resolve.Go.Expression as Expression (resolve)
import qualified Semantic.Resolve.Go.Statements as Statements (resolve)
import Semantic.Stage (Resolve)
import Semantic.Tree.Body (Body (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.Body as Syntax (Body (..))

resolve :: Context scope -> Syntax.Body Position -> Body Normal Resolve scope
resolve context = \case
  Syntax.Body {expression} -> Body (Expression.resolve context expression)
  Syntax.Guards {statements} -> Guards (fmap (Statements.resolve context) statements)
