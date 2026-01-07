module Stage3.Temporary.Lambda where

import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Pattern)
import Stage3.Temporary.Expression (Expression)
import Stage3.Temporary.Pattern (Pattern)

data Lambda s scope
  = Plain !(Expression s scope)
  | Bound !(Pattern s scope) (Lambda s (Scope.Pattern ':+ scope))
