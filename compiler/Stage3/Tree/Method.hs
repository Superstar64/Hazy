module Stage3.Tree.Method where

import Stage3.Tree.Scheme (Scheme)
import qualified Stage4.Tree.Scheme as Simple

data Method scope = Method
  { annotation :: !(Scheme scope),
    annotation' :: !(Simple.Scheme scope)
  }
  deriving (Show)
