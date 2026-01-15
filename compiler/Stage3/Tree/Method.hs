module Stage3.Tree.Method where

import qualified Data.Strict.Maybe as Strict
import Stage2.Scope (Environment (..), Local)
import qualified Stage3.Simple.Scheme as Simple
import Stage3.Tree.Definition (Definition)
import Stage3.Tree.Scheme (Scheme)

data Method scope = Method
  { annotation :: !(Scheme scope),
    annotation' :: !(Simple.Scheme scope),
    definition :: !(Strict.Maybe (Definition (Local ':+ scope)))
  }
  deriving (Show)
