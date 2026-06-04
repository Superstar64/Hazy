module Stage2.Resolve.Go.Body where

import Stage1.Position (Position)
import qualified Stage1.Tree.Body as Stage1 (Body (..))
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context)
import {-# SOURCE #-} qualified Stage2.Resolve.Go.Expression as Expression (resolve)
import qualified Stage2.Resolve.Go.Statements as Statements (resolve)
import Stage2.Stage (Resolve)
import Stage2.Tree.Body (Body (..))

resolve :: Context scope -> Stage1.Body Position -> Body Normal Resolve scope
resolve context = \case
  Stage1.Body {expression} -> Body (Expression.resolve context expression)
  Stage1.Guards {statements} -> Guards (fmap (Statements.resolve context) statements)
