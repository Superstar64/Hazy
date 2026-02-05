module Stage5.Tree.Expression where

import Control.Monad.ST (ST)
import qualified Javascript.Tree.Expression as Javascript (Expression)
import qualified Javascript.Tree.Statement as Javascript (Statement)
import Stage4.Tree.Expression (Expression)
import Stage4.Tree.SchemeOver (SchemeOver)
import Stage5.Generate.Context (Context)

generate ::
  Context s scope ->
  Expression scope ->
  ST s ([Javascript.Statement 'True], Javascript.Expression)
generateInto ::
  Context s scope ->
  Javascript.Expression ->
  Expression scope ->
  ST s [Javascript.Statement 'True]
thunk :: Context s scope -> Expression scope -> ST s Javascript.Expression
declaration :: Context s scope -> SchemeOver Expression scope -> ST s Javascript.Expression
