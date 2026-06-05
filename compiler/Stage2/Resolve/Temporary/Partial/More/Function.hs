module Stage2.Resolve.Temporary.Partial.More.Function where

import Stage2.Layout (Normal)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Stage (Resolve)
import Stage2.Tree.Function as Tree (Function)

data Function scope = Function
  { functionAuto :: Tree.Function Normal Resolve scope,
    functionManual :: Tree.Function Normal Resolve (Local ':+ scope)
  }
