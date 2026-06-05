module Semantic.Resolve.Temporary.Partial.More.Function where

import Semantic.Layout (Normal)
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Stage (Resolve)
import Semantic.Tree.Function as Tree (Function)

data Function scope = Function
  { functionAuto :: Tree.Function Normal Resolve scope,
    functionManual :: Tree.Function Normal Resolve (Local ':+ scope)
  }
