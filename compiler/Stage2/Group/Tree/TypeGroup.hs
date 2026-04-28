module Stage2.Group.Tree.TypeGroup where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Graph.StronglyConnected as StronglyConnected
import Stage2.Group.Tree.TypeDefinition2 (TypeDefinition2)
import qualified Stage2.Group.Tree.TypeDefinition2 as TypeDefinition2
import qualified Stage2.Index.Link.Type as Type
import Stage2.Tree.TypeDeclaration (TypeDeclaration)

data TypeGroup locality scope
  = Group
      { link :: !(Type.Link locality),
        set :: !(Map (Type.Link locality) (TypeDefinition2 scope))
      }
  | Link
      { link :: !(Type.Link locality)
      }
  deriving (Show)

group ::
  (Type.Link locality -> TypeDeclaration locality scope) ->
  StronglyConnected.Component (Type.Link locality) ->
  TypeGroup locality scope
group index = \case
  StronglyConnected.Group {link, set} ->
    Group
      { link,
        set = Map.fromSet (TypeDefinition2.group . index) set
      }
  StronglyConnected.Link {link} -> Link {link}
