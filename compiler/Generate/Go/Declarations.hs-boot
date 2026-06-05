module Generate.Go.Declarations where

import Control.Monad.ST (ST)
import Core.Tree.Declarations (Declarations)
import Generate.Context (Context)
import qualified Javascript.Tree.Statement as Javascript
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope

generate ::
  Context s scope ->
  Declarations (Scope.Declaration ':+ scope) ->
  ST s (Context s (Scope.Declaration ':+ scope), [Javascript.Statement local])
