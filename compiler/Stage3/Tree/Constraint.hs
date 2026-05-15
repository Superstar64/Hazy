module Stage3.Tree.Constraint where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Stage (Check)
import Stage2.Tree.Type (Type)

data Constraint scope = Constraint
  { classx :: !(Type2.Index scope),
    head :: !Int,
    arguments :: !(Strict.Vector (Type Position Check (Local ':+ scope)))
  }
  deriving (Show)
