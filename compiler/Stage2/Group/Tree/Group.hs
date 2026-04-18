module Stage2.Group.Tree.Group where

import Data.Map (Map)
import qualified Stage2.Group.Index.Term0 as Term0
import Stage2.Group.Tree.Definition3 (Definition3)

data Group scope
  = Group
      { group :: !(Map (Term0.Index scope) (Definition3 scope))
      }
  | Link
      { link :: !(Term0.Index scope)
      }
  deriving (Show)
