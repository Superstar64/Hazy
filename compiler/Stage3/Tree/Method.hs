module Stage3.Tree.Method where

import Stage1.Position (Position)
import Stage2.Stage (Check)
import Stage3.Tree.Scheme (Scheme)
import qualified Stage4.Tree.Scheme as Simple

data Method scope = Method
  { annotation :: !(Scheme Position Check scope),
    annotation' :: !(Simple.Scheme scope)
  }
  deriving (Show)
