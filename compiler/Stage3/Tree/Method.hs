module Stage3.Tree.Method where

import qualified Stage3.Simple.Scheme as Simple
import Stage3.Tree.Scheme (Scheme)

data Method scope = Method
  { annotation :: !(Scheme scope),
    annotation' :: !(Simple.Scheme scope)
  }
  deriving (Show)
