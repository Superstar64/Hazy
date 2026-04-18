module Stage2.Group.Tree.TypeGroup where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Graph.StronglyConnected as StronglyConnected
import Stage2.Group.Tree.TypeDefinition2 (TypeDefinition2)
import qualified Stage2.Group.Tree.TypeDefinition2 as TypeDefinition2
import qualified Stage2.Index.Type0 as Type0
import Stage2.Tree.TypeDeclaration (TypeDeclaration)

data TypeGroup scope
  = Group
      { set :: !(Map (Type0.Index scope) (TypeDefinition2 scope))
      }
  | Link
      { link :: !(Type0.Index scope)
      }
  deriving (Show)

group ::
  (Type0.Index scope -> TypeDeclaration scope) ->
  StronglyConnected.Component (Type0.Index scope) ->
  TypeGroup scope
group index = \case
  StronglyConnected.Group set -> Group {set = Map.fromSet (TypeDefinition2.group . index) set}
  StronglyConnected.Link link -> Link {link}
