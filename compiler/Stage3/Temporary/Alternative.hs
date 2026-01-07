module Stage3.Temporary.Alternative where

import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Pattern)
import Stage3.Temporary.Pattern (Pattern)
import Stage3.Temporary.RightHandSide (RightHandSide)

data Alternative s scope
  = Alternative !(Pattern s scope) !(RightHandSide s (Scope.Pattern ':+ scope))
