module Stage2.Group.Tree.Group where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Graph.StronglyConnected as StronglyConnected
import qualified Stage2.Group.Index.Link.Term as Term
import qualified Stage2.Group.Temporary.Declaration as Temporary
import Stage2.Group.Tree.Definition3 (Definition3)
import qualified Stage2.Group.Tree.Definition3 as Definition3

data Group locality scope
  = Group
      { link :: !(Term.Link locality),
        set :: !(Map (Term.Link locality) (Definition3 scope))
      }
  | Link
      { link :: !(Term.Link locality)
      }
  deriving (Show)

group ::
  (Term.Link locality -> Temporary.Declaration scope) ->
  StronglyConnected.Component (Term.Link locality) ->
  Group locality scope
group index = \case
  StronglyConnected.Group {link, set} ->
    Group
      { link,
        set = Map.fromSet (Definition3.group . index) set
      }
  StronglyConnected.Link {link} -> Link {link}
