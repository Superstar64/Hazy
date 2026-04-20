module Stage3.Tree.Shared where

import Stage1.Position (Position)
import Stage3.Tree.RightHandSide2 (RightHandSide2)
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver)

data Shared scope = Shared
  { body :: !(Simple.SchemeOver RightHandSide2 scope),
    equalPosition :: !Position
  }
  deriving (Show)
