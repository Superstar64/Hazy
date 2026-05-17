module Stage3.Tree.Method where

import Stage1.Position (Position)
import Stage2.Stage (Check)
import Stage3.Tree.Scheme (Scheme)

newtype Method scope = Method
  { annotation :: Scheme Position Check scope
  }
  deriving (Show)
