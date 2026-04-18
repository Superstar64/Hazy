module Stage2.Group.Tree.Group where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Graph.StronglyConnected as StronglyConnected
import qualified Stage2.Group.Index.Term0 as Term0
import qualified Stage2.Group.Temporary.Declaration as Temporary
import Stage2.Group.Tree.Definition3 (Definition3)
import qualified Stage2.Group.Tree.Definition3 as Definition3

data Group scope
  = Group
      { set :: !(Map (Term0.Index scope) (Definition3 scope))
      }
  | Link
      { link :: !(Term0.Index scope)
      }
  deriving (Show)

group ::
  (Term0.Index scope -> Temporary.Declaration scope) ->
  StronglyConnected.Component (Term0.Index scope) ->
  Group scope
group index = \case
  StronglyConnected.Group set -> Group {set = Map.fromSet (Definition3.group . index) set}
  StronglyConnected.Link link -> Link {link}
