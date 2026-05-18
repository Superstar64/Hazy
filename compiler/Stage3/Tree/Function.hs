module Stage3.Tree.Function where

import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Pattern)
import Stage2.Stage (Check)
import Stage2.Tree.Pattern (Pattern)
import Stage3.Tree.RightHandSide (RightHandSide)

data Function scope
  = Plain {plain :: !(RightHandSide scope)}
  | Bound
      { patternx :: !(Pattern Check scope),
        body :: !(Function (Scope.Pattern ':+ scope))
      }
  deriving (Show)
