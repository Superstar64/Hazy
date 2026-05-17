module Stage3.Tree.Instance where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Stage (Check)
import Stage2.Tree.Constraint (Constraint)
import Stage2.Tree.TypePattern (TypePattern)
import Stage3.Tree.InstanceMethod (InstanceMethod (..))
import qualified Stage4.Tree.Evidence as Simple (Evidence)
import Prelude hiding (head)

data Instance scope = Instance
  { parameters :: !(Strict.Vector (TypePattern Position Check scope)),
    prerequisites :: !(Strict.Vector (Constraint Position Check scope)),
    evidence :: !(Strict.Vector (Simple.Evidence (Local ':+ scope))),
    members :: !(Strict.Vector (InstanceMethod scope))
  }
  deriving (Show)
