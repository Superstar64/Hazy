module Generate.Target where

import qualified Javascript.Tree.Expression as Javascript (Expression (..))
import qualified Javascript.Tree.Statement as Javascript (Statement (..))

data Target return where
  Assign :: !Javascript.Expression -> Target 'False
  Return :: Target 'True

finish :: Target return -> Javascript.Expression -> Javascript.Statement 'True
finish target value = case target of
  Assign target ->
    Javascript.Expression
      ( Javascript.Assign
          { target,
            value
          }
      )
  Return -> Javascript.Return value
