module Stage3.Tree.RightHandSide where

import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Declaration)
import Stage3.Tree.Body (Body)
import {-# SOURCE #-} Stage3.Tree.Declarations (Declarations)

data RightHandSide scope
  = RightHandSide
  { body :: !(Body (Scope.Declaration ':+ scope)),
    declarations :: !(Declarations (Scope.Declaration ':+ scope))
  }
  deriving (Show)
