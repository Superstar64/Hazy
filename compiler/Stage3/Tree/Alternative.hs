module Stage3.Tree.Alternative where

import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Stage (Check)
import Stage2.Tree.Pattern (Pattern)
import Stage3.Tree.RightHandSide (RightHandSide)

data Alternative scope
  = Alternative
  { parameter :: !(Pattern Check scope),
    rightHandSide :: !(RightHandSide (Scope.Pattern ':+ scope))
  }
  deriving (Show)
