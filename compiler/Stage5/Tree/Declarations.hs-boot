module Stage5.Tree.Declarations where

import Control.Monad.ST (ST)
import qualified Javascript.Tree.Statement as Javascript
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage4.Tree.Declarations (Declarations)
import Stage5.Generate.Context (Context)

generate ::
  Context s scope ->
  Declarations (Scope.Declaration ':+ scope) ->
  ST s (Context s (Scope.Declaration ':+ scope), [Javascript.Statement local])
