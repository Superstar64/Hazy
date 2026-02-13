module Stage3.Tree.Shared where

import Stage1.Position (Position)
import qualified Stage2.Scope as Scope
import Stage3.Tree.RightHandSide (RightHandSide)
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver)
import qualified Stage4.Tree.Type as Simple (Type)

data Shared scope = Shared
  { body :: !(Simple.SchemeOver Body scope),
    equalPosition :: !Position
  }
  deriving (Show)

data Body scope = Body
  { definition :: !(RightHandSide scope),
    typex :: !(Simple.Type scope)
  }
  deriving (Show)

instance Scope.Show Body where
  showsPrec = showsPrec
