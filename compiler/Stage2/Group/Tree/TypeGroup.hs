module Stage2.Group.Tree.TypeGroup where

import Data.Map (Map)
import Stage2.Group.Tree.TypeDefinition2 (TypeDefinition2)
import qualified Stage2.Index.Type0 as Type0

data TypeGroup scope
  = Group
      { group :: !(Map (Type0.Index scope) (TypeDefinition2 scope))
      }
  | Link
      { link :: !(Type0.Index scope)
      }
  deriving (Show)
