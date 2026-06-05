module Generate.Go.Expression where

import Control.Monad.ST (ST)
import Core.Tree.Expression (Expression)
import Core.Tree.SchemeOver (SchemeOver)
import Generate.Context (Context)
import qualified Javascript.Tree.Expression as Javascript (Expression)
import qualified Javascript.Tree.Statement as Javascript (Statement)

generate ::
  Context s scope ->
  Expression scope ->
  ST s ([Javascript.Statement 'True], Javascript.Expression)
generateInto ::
  Context s scope ->
  Javascript.Expression ->
  Expression scope ->
  ST s [Javascript.Statement 'True]

data Binder
  = Done
  | Group

force :: Javascript.Expression -> Javascript.Expression
thunk :: Context s scope -> Binder -> Expression scope -> ST s Javascript.Expression
declaration :: Context s scope -> SchemeOver Expression scope -> ST s Javascript.Expression
