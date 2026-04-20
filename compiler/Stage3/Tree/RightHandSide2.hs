module Stage3.Tree.RightHandSide2 where

import qualified Stage2.Scope as Scope
import Stage3.Tree.RightHandSide (RightHandSide)
import qualified Stage4.Tree.Type as Simple

data RightHandSide2 scope = RightHandSide2
  { definition :: !(RightHandSide scope),
    typex :: !(Simple.Type scope)
  }
  deriving (Show)

instance Scope.Show RightHandSide2 where
  showsPrec = showsPrec
