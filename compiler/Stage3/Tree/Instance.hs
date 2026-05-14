module Stage3.Tree.Instance where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage2.Scope (Environment ((:+)), Local)
import Stage3.Tree.InstanceMethod (InstanceMethod (..))
import qualified Stage4.Tree.Evidence as Simple (Evidence)
import Prelude hiding (head)

data Instance scope = Instance
  { evidence :: !(Strict.Vector (Simple.Evidence (Local ':+ scope))),
    prerequisitesCount :: !Int,
    members :: !(Strict.Vector (InstanceMethod scope))
  }
  deriving (Show)
