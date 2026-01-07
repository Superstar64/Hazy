module Stage2.Temporary.Partial.More.Function where

import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Tree.Function as Tree (Function)

data Function scope = Function
  { functionAuto :: Tree.Function scope,
    functionManual :: Tree.Function (Local ':+ scope)
  }
