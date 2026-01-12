module Stage3.Tree.Alternative where

import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage3.Tree.Pattern (Pattern)
import Stage3.Tree.RightHandSide (RightHandSide)

data Alternative scope
  = Alternative
  { parameter :: !(Pattern scope),
    rightHandSide :: !(RightHandSide (Scope.Pattern ':+ scope))
  }
  deriving (Show)
